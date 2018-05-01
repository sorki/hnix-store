{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}

{-|
Description : Allowed effects for interacting with Nar files.
Maintainer  : Shea Levy <shea@shealevy.com>
|-}
module System.Nix.Nar where

import           Control.Applicative
import           Control.Monad              (replicateM, replicateM_)
import qualified Data.Binary                as B
import qualified Data.Binary.Get            as B
import qualified Data.Binary.Put            as B
import qualified Data.ByteString.Lazy.Char8 as BSL
import           Data.Maybe                 (fromMaybe)
import           Data.Monoid                ((<>))
import qualified Data.Set                   as Set
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as E
import           GHC.Int                    (Int64)

import           System.Nix.Path


data NarEffects (m :: * -> *) = NarEffets {
    readFile         :: FilePath -> m BSL.ByteString
  , listDir          :: FilePath -> m [FileSystemObject]
  , narFromFileBytes :: BSL.ByteString -> m Nar
  , narFromDirectory :: FilePath -> m Nar
}


-- Directly taken from Eelco thesis
-- https://nixos.org/%7Eeelco/pubs/phd-thesis.pdf

-- TODO: Should we use rootedPath, validPath rather than FilePath?
data Nar = Nar { narFile :: FileSystemObject }
    deriving (Eq, Ord, Show)

data FileSystemObject =
    Regular IsExecutable BSL.ByteString
  | Directory (Set.Set (PathName, FileSystemObject))
  | SymLink BSL.ByteString
  deriving (Eq, Show)

-- TODO - is this right? How does thesis define ordering of FSOs?
instance Ord FileSystemObject where
    compare (Regular _ c1) (Regular _ c2) = compare c1 c2
    compare (Regular _ _)  _              = GT
    compare (Directory s1) (Directory s2) = compare s1 s2
    compare (Directory _)  _              = GT
    compare (SymLink l1) (SymLink l2)     = compare l1 l2

data IsExecutable = NonExecutable | Executable
    deriving (Eq, Show)


------------------------------------------------------------------------------
-- | Serialize Nar to lazy ByteString
putNar :: Nar -> B.Put
putNar (Nar file) = header <>
                    parens (putFile file)
    where

        header   = str "nix-archive-1"

        putFile (Regular isExec contents) =
               str "type" >> str "regular"
            >> (if isExec == Executable
               then str "executable" <> str ""
               else return ())
            >> str "contents" <> str contents

        putFile (SymLink target) =
               str "type" >> str "symlink" >> str "target" >> str target

        putFile (Directory entries) =
               str "type" <> str "directory"
            <> foldMap putEntry entries

        putEntry (PathName name, fso) =
            str "entry" <>
            parens (str "name" <>
                    str (BSL.fromStrict $ E.encodeUtf8 name) <>
                    str "node" <>
                    parens (putFile fso))

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
          return $ Directory (Set.fromList fs)

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
              maybe (fail $ "Bad PathName: " ++ show mname)
                    (return . (,file))
                    (pathName mname)

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
