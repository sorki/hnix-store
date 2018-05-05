{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}

{-|
Description : Allowed effects for interacting with Nar files.
Maintainer  : Shea Levy <shea@shealevy.com>
|-}
module System.Nix.Nar (
    FileSystemObject(..)
  , IsExecutable (..)
  , Nar(..)
  , getNar
  , localUnpackNar
  , putNar
  ) where

import           Control.Applicative
import           Control.Monad              (replicateM, replicateM_)
import Control.Monad.Writer
import qualified Data.Binary                as B
import qualified Data.Binary.Get            as B
import qualified Data.Binary.Put            as B
import qualified Data.ByteString.Lazy.Char8 as BSL
import           Data.Foldable              (forM_)
import qualified Data.Map                   as Map
import           Data.Maybe                 (fromMaybe)
import           Data.Monoid                ((<>))
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as E
import           GHC.Int                    (Int64)
import           System.Directory
import           System.FilePath
import           System.Posix.Files         (createSymbolicLink)

import           System.Nix.Path


data NarEffects (m :: * -> *) = NarEffets {
    readFile :: FilePath -> m BSL.ByteString
  , listDir  :: FilePath -> m [FileSystemObject]
}


-- Directly taken from Eelco thesis
-- https://nixos.org/%7Eeelco/pubs/phd-thesis.pdf

data Nar = Nar { narFile :: FileSystemObject }
    deriving (Eq, Show)

data FileSystemObject =
    Regular IsExecutable BSL.ByteString
  | Directory (Map.Map FilePathPart FileSystemObject)
  | SymLink BSL.ByteString
  deriving (Eq, Show)

-- -- TODO - is this right? How does thesis define ordering of FSOs?
-- instance Ord FileSystemObject where
--     compare (Regular _ c1) (Regular _ c2) = compare c1 c2
--     compare (Regular _ _)  _              = GT
--     compare (Directory s1) (Directory s2) = compare s1 s2
--     compare (Directory _)  _              = GT
--     compare (SymLink l1) (SymLink l2)     = compare l1 l2

data IsExecutable = NonExecutable | Executable
    deriving (Eq, Show)


------------------------------------------------------------------------------
-- | Serialize Nar to lazy ByteString
putNar :: Nar -> B.Put
putNar (Nar file) = header <> parens (putFile file)
    where

        header   = str "nix-archive-1"

        putFile (Regular isExec contents) =
               strs ["type", "regular"]
            >> (if isExec == Executable
               then strs ["executable", ""]
               else return ())
            >> strs ["contents", contents]

        putFile (SymLink target) =
               strs ["type", "symlink", "target", target]

        -- toList sorts the entries by FilePathPart before serializing
        putFile (Directory entries) =
               strs ["type", "directory"]
            <> mapM_ putEntry (Map.toList entries)

        putEntry (FilePathPart name, fso) = do
            str "entry"
            parens $ do
              str "name"
              str (BSL.fromStrict $ E.encodeUtf8 name)
              str "node"
              parens (putFile fso)

        parens m = str "(" >> m >> str ")"

        str :: BSL.ByteString -> B.Put
        str t = let len = BSL.length t
            in int len <> pad t

        int :: Integral a => a -> B.Put
        int n = B.putInt64le $ fromIntegral n

        pad :: BSL.ByteString -> B.Put
        pad bs = do
          B.putLazyByteString bs
          B.putLazyByteString (BSL.replicate (padLen (BSL.length bs)) '\NUL')

        strs :: [BSL.ByteString] -> B.Put
        strs = mapM_ str


------------------------------------------------------------------------------
-- | Deserialize a Nar from lazy ByteString
getNar :: B.Get Nar
getNar = fmap Nar $ header >> parens getFile
    where

      header   = assertStr "nix-archive-1"


      -- Fetch a FileSystemObject
      getFile = getRegularFile <|> getDirectory <|> getSymLink

      getRegularFile = do
          assertStr "type"
          assertStr "regular"
          mExecutable <- optional $ Executable <$ (assertStr "executable"
                                                   >> assertStr "")
          assertStr "contents"
          contents <- str
          return $ Regular (fromMaybe NonExecutable mExecutable) contents

      getDirectory = do
          assertStr "type"
          assertStr "directory"
          fs <- many getEntry
          return $ Directory (Map.fromList fs)

      getSymLink = do
          assertStr "type"
          assertStr "symlink"
          assertStr "target"
          fmap SymLink str

      getEntry = do
          assertStr "entry"
          parens $ do
              assertStr "name"
              mname <- E.decodeUtf8 . BSL.toStrict <$> str
              assertStr "node"
              file <- parens getFile
              maybe (fail $ "Bad FilePathPart: " ++ show mname)
                    (return . (,file))
                    (filePathPart mname)

      -- Fetch a length-prefixed, null-padded string
      str = do
          n <- B.getInt64le
          s <- B.getLazyByteString n
          p <- B.getByteString . fromIntegral $ padLen n
          return s

      parens m = assertStr "(" *> m <* assertStr ")"

      assertStr s = do
          s' <- str
          if s == s'
              then return s
              else fail "No"


-- | Distance to the next multiple of 8
padLen :: Int64 -> Int64
padLen n = (8 - n) `mod` 8


-- | Unpack a FileSystemObject into a non-nix-store directory (e.g. for testing)
localUnpackNar :: FilePath -> Nar -> IO ()
localUnpackNar basePath (Nar fso) = do

  -- Create regular files and directories,
  -- But save link creation until the end, by writing link commands out
  -- to a queue, since otherwise we may try to create a link before
  -- its target exists

  -- TODO: Links may link to each other, so find a topological sort
  -- of all the enqueued links. Otherwise we may still create them
  -- in an invalid order. Ugh. :)
  links <- execWriterT $ localUnpackFSO basePath fso
  mapM_ (uncurry createSymbolicLink) links

  where
    localUnpackFSO :: FilePath
                   -> FileSystemObject
                   -> WriterT [(FilePath, FilePath)] IO ()
    localUnpackFSO basePath fso = case fso of

       Regular isExec bs -> liftIO $ do
         BSL.writeFile basePath bs
         p <- getPermissions basePath
         setPermissions basePath (p {executable = isExec == Executable})

       SymLink targ -> tell [(BSL.unpack targ,  basePath)]

       Directory contents -> do
         liftIO $ createDirectory basePath
         forM_ (Map.toList contents) $ \(FilePathPart path', fso) ->
           localUnpackFSO (basePath </> T.unpack path') fso
