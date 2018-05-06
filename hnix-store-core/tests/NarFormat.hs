{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module NarFormat where

import           Control.Exception           (SomeException, try)
import           Control.Monad               (replicateM)
import           Control.Monad.IO.Class      (liftIO)
import           Data.Binary.Get             (Get (..), runGet)
import           Data.Binary.Put             (Put (..), runPut)
import qualified Data.ByteString.Base64.Lazy as B64
import qualified Data.ByteString.Lazy        as BSL
import qualified Data.ByteString.Lazy.Char8  as BSC
import qualified Data.Map                    as Map
import           Data.Maybe                  (isJust)
import qualified Data.Text                   as T
import qualified System.Process              as P
import           Test.Tasty                  as T
import           Test.Tasty.Hspec
import qualified Test.Tasty.HUnit            as HU
import           Test.Tasty.QuickCheck

import           System.Nix.Nar
import           System.Nix.Path



spec_narEncoding :: Spec
spec_narEncoding = do

  -- For a Haskell embedded Nar, check that (decode . encode === id)
  let roundTrip n = runGet getNar (runPut $ putNar n) `shouldBe` n

  -- For a Haskell embedded Nar, check that encoding it gives
  -- the same bytestring as `nix-store --dump`
  let encEqualsNixStore n b = runPut (putNar n) `shouldBe` b


  describe "parser-roundtrip" $ do
    it "roundtrips regular" $ do
      roundTrip (Nar sampleRegular)

    it "roundtrips regular 2" $ do
      roundTrip (Nar sampleRegular')

    it "roundtrips executable" $ do
      roundTrip (Nar sampleExecutable)

    it "roundtrips symlink" $ do
      roundTrip (Nar sampleSymLink)

    it "roundtrips directory" $ do
      roundTrip (Nar sampleDirectory)


  describe "matches-nix-store fixture" $ do
    it "matches regular" $ do
      encEqualsNixStore (Nar sampleRegular) sampleRegularBaseline

    it "matches regular'" $
      encEqualsNixStore (Nar sampleRegular') sampleRegular'Baseline

    it "matches executable" $
      encEqualsNixStore (Nar sampleExecutable) sampleExecutableBaseline

    it "matches symlink" $
      encEqualsNixStore (Nar sampleSymLink) sampleSymLinkBaseline

    it "matches directory" $ do
      encEqualsNixStore (Nar sampleDirectory) sampleDirectoryBaseline

unit_nixStoreRegular :: HU.Assertion
unit_nixStoreRegular = filesystemNixStore "regular" (Nar sampleRegular)

unit_nixStoreDirectory :: HU.Assertion
unit_nixStoreDirectory = filesystemNixStore "directory" (Nar sampleDirectory)

unit_nixStoreDirectory' :: HU.Assertion
unit_nixStoreDirectory' = filesystemNixStore "directory'" (Nar sampleDirectory')

unit_nixStoreBigFile :: HU.Assertion
unit_nixStoreBigFile = filesystemNixStore "bigfile'" (Nar sampleLargeFile)

unit_nixStoreBigDir :: HU.Assertion
unit_nixStoreBigDir = filesystemNixStore "bigfile'" (Nar sampleLargeDir)

prop_narEncodingArbitrary :: Nar -> Property
prop_narEncodingArbitrary n = runGet getNar (runPut $ putNar n) === n


-- | Generate the ground-truth encoding on the fly with
--   `nix-store --dump`, rather than generating fixtures
--   beforehand
filesystemNixStore :: String -> Nar -> IO ()
filesystemNixStore s n = do

  ver <- try (P.readProcess "nix-store" ["--version"] "")
  case ver of
    Left  (e :: SomeException) -> print "No nix-store on system"
    Right _ -> do
      localUnpackNar narEffectsIO "testfile" n
      nixStoreNar <- P.readProcess "nix-store" ["--dump", "testfile"] ""
      _ <- P.runCommand "rm -rf testfile"
      HU.assertEqual s (runPut (putNar n)) (BSC.pack nixStoreNar)




-- * Several sample FSOs defined in Haskell, for use in encoding/decoding

-- | Simple regular text file with contents 'hi'
sampleRegular :: FileSystemObject
sampleRegular = Regular NonExecutable "hi\n"

-- | Simple text file with some c code
sampleRegular' :: FileSystemObject
sampleRegular' = Regular NonExecutable
  "#include <stdio.h>\n\nint main(int argc, char *argv[]){ exit 0; }\n"

-- | Executable file
sampleExecutable :: FileSystemObject
sampleExecutable = Regular Executable
  "#!/bin/bash\n\ngcc -o hello hello.c\n"

-- | A simple symlink
sampleSymLink :: FileSystemObject
sampleSymLink = SymLink "hello.c"


-- | A directory that includes some of the above sample files
sampleDirectory :: FileSystemObject
sampleDirectory = Directory $ Map.fromList
  [(FilePathPart "hello.c", sampleRegular')
  ,(FilePathPart "build.sh", sampleExecutable)
  ,(FilePathPart "hi.c", sampleSymLink)
  ]

-- | A deeper directory tree with crossing links
sampleDirectory' :: FileSystemObject
sampleDirectory' = Directory $ Map.fromList [

    (FilePathPart "foo", Directory $ Map.fromList [
        (FilePathPart "foo.txt", Regular NonExecutable "foo text")
      , (FilePathPart "tobar"  , SymLink "../bar/bar.txt")
      ])

  , (FilePathPart "bar", Directory $ Map.fromList [
        (FilePathPart "bar.txt", Regular NonExecutable "bar text")
      , (FilePathPart "tofoo"  , SymLink "../foo/foo.txt")
      ])
  ]

-- NOTE: The `nixStoreBigFile` test fails at 9000000 bytes
sampleLargeFile :: FileSystemObject
sampleLargeFile =
  Regular NonExecutable (BSL.take 9000000 (BSL.cycle "Lorem ipsum "))


-- NOTE: The `nixStoreBigDir` test fails at 9000000 bytes with the error:
--  nixStoreBigDir:            error: writing to file: Broken pipe
--  FAIL
--  Exception: fd:5: hGetContents: invalid argument (invalid byte sequence)
sampleLargeFile' :: FileSystemObject
sampleLargeFile' =
  Regular NonExecutable (BSL.take 9000000 (BSL.cycle "Lorems ipsums "))

sampleLargeDir :: FileSystemObject
sampleLargeDir = Directory $ Map.fromList $ [
    (FilePathPart "bf1", sampleLargeFile)
  , (FilePathPart "bf2", sampleLargeFile')
  ]
  ++ [ (FilePathPart (T.pack $ 'f' : show n),
        Regular NonExecutable (BSL.take 10000 (BSL.cycle "hi ")))
     | n <- [1..100]]
  ++ [
  (FilePathPart "d", Directory $ Map.fromList
      [ (FilePathPart (T.pack $ "df" ++ show n)
        , Regular NonExecutable (BSL.take 10000 (BSL.cycle "subhi ")))
      | n <- [1..100]]
     )
  ]

-- * For each sample above, feed it into `nix-store --dump`,
-- and base64 encode the resulting NAR binary. This lets us
-- check our Haskell NAR generator against `nix-store`

-- "hi" file turned to a NAR with `nix-store --dump`, Base64 encoded
sampleRegularBaseline :: BSL.ByteString
sampleRegularBaseline = B64.decodeLenient
  "DQAAAAAAAABuaXgtYXJjaGl2ZS0xAAAAAQAAAAAAAAAoAAAAAAA\
  \AAAQAAAAAAAAAdHlwZQAAAAAHAAAAAAAAAHJlZ3VsYXIACAAAAA\
  \AAAABjb250ZW50cwMAAAAAAAAAaGkKAAAAAAABAAAAAAAAACkAA\
  \AAAAAAA"

sampleRegular'Baseline :: BSL.ByteString
sampleRegular'Baseline = B64.decodeLenient
  "DQAAAAAAAABuaXgtYXJjaGl2ZS0xAAAAAQAAAAAAAAAoAAAAAAA\
  \AAAQAAAAAAAAAdHlwZQAAAAAHAAAAAAAAAHJlZ3VsYXIACAAAAA\
  \AAAABjb250ZW50c0AAAAAAAAAAI2luY2x1ZGUgPHN0ZGlvLmg+C\
  \gppbnQgbWFpbihpbnQgYXJnYywgY2hhciAqYXJndltdKXsgZXhp\
  \dCAwOyB9CgEAAAAAAAAAKQAAAAAAAAA="

sampleExecutableBaseline :: BSL.ByteString
sampleExecutableBaseline = B64.decodeLenient
  "DQAAAAAAAABuaXgtYXJjaGl2ZS0xAAAAAQAAAAAAAAAoAAAAAAA\
  \AAAQAAAAAAAAAdHlwZQAAAAAHAAAAAAAAAHJlZ3VsYXIACgAAAA\
  \AAAABleGVjdXRhYmxlAAAAAAAAAAAAAAAAAAAIAAAAAAAAAGNvb\
  \nRlbnRzIgAAAAAAAAAjIS9iaW4vYmFzaAoKZ2NjIC1vIGhlbGxv\
  \IGhlbGxvLmMKAAAAAAAAAQAAAAAAAAApAAAAAAAAAA=="

sampleSymLinkBaseline :: BSL.ByteString
sampleSymLinkBaseline = B64.decodeLenient
  "DQAAAAAAAABuaXgtYXJjaGl2ZS0xAAAAAQAAAAAAAAAoAAAAAAA\
  \AAAQAAAAAAAAAdHlwZQAAAAAHAAAAAAAAAHN5bWxpbmsABgAAAA\
  \AAAAB0YXJnZXQAAAcAAAAAAAAAaGVsbG8uYwABAAAAAAAAACkAA\
  \AAAAAAA"

sampleDirectoryBaseline :: BSL.ByteString
sampleDirectoryBaseline = B64.decodeLenient
  "DQAAAAAAAABuaXgtYXJjaGl2ZS0xAAAAAQAAAAAAAAAoAAAAAAA\
  \AAAQAAAAAAAAAdHlwZQAAAAAJAAAAAAAAAGRpcmVjdG9yeQAAAA\
  \AAAAAFAAAAAAAAAGVudHJ5AAAAAQAAAAAAAAAoAAAAAAAAAAQAA\
  \AAAAAAAbmFtZQAAAAAIAAAAAAAAAGJ1aWxkLnNoBAAAAAAAAABu\
  \b2RlAAAAAAEAAAAAAAAAKAAAAAAAAAAEAAAAAAAAAHR5cGUAAAA\
  \ABwAAAAAAAAByZWd1bGFyAAoAAAAAAAAAZXhlY3V0YWJsZQAAAA\
  \AAAAAAAAAAAAAACAAAAAAAAABjb250ZW50cyIAAAAAAAAAIyEvY\
  \mluL2Jhc2gKCmdjYyAtbyBoZWxsbyBoZWxsby5jCgAAAAAAAAEA\
  \AAAAAAAAKQAAAAAAAAABAAAAAAAAACkAAAAAAAAABQAAAAAAAAB\
  \lbnRyeQAAAAEAAAAAAAAAKAAAAAAAAAAEAAAAAAAAAG5hbWUAAA\
  \AABwAAAAAAAABoZWxsby5jAAQAAAAAAAAAbm9kZQAAAAABAAAAA\
  \AAAACgAAAAAAAAABAAAAAAAAAB0eXBlAAAAAAcAAAAAAAAAcmVn\
  \dWxhcgAIAAAAAAAAAGNvbnRlbnRzQAAAAAAAAAAjaW5jbHVkZSA\
  \8c3RkaW8uaD4KCmludCBtYWluKGludCBhcmdjLCBjaGFyICphcm\
  \d2W10peyBleGl0IDA7IH0KAQAAAAAAAAApAAAAAAAAAAEAAAAAA\
  \AAAKQAAAAAAAAAFAAAAAAAAAGVudHJ5AAAAAQAAAAAAAAAoAAAA\
  \AAAAAAQAAAAAAAAAbmFtZQAAAAAEAAAAAAAAAGhpLmMAAAAABAA\
  \AAAAAAABub2RlAAAAAAEAAAAAAAAAKAAAAAAAAAAEAAAAAAAAAH\
  \R5cGUAAAAABwAAAAAAAABzeW1saW5rAAYAAAAAAAAAdGFyZ2V0A\
  \AAHAAAAAAAAAGhlbGxvLmMAAQAAAAAAAAApAAAAAAAAAAEAAAAA\
  \AAAAKQAAAAAAAAABAAAAAAAAACkAAAAAAAAA"


-- | Add a link to a FileSystemObject. This is useful
--   when creating Arbitrary FileSystemObjects. It
--   isn't implemented yet
mkLink ::
     FilePath -- ^ Target
  -> FilePath -- ^ Link
  -> FileSystemObject -- ^ FileSystemObject to add link to
  -> FileSystemObject
mkLink = undefined -- TODO


instance Arbitrary Nar where
  arbitrary = Nar <$> resize 10 arbitrary

instance Arbitrary FileSystemObject where
  -- To build an arbitrary Nar,
  arbitrary = do
    n <- getSize
    if n < 2
      then arbFile
      else arbDirectory n

      where

        arbFile :: Gen FileSystemObject
        arbFile = Regular
         <$> elements [NonExecutable, Executable]
         <*> oneof  [fmap BSL.pack (arbitrary) , -- Binary File
                     fmap BSC.pack (arbitrary) ] -- ASCII  File

        arbName :: Gen FilePathPart
        arbName = fmap (FilePathPart . T.pack) $ do
          Positive n <- arbitrary
          replicateM n (elements $ ['a'..'z'] ++ ['0'..'9'])

        arbDirectory :: Int -> Gen FileSystemObject
        arbDirectory n = fmap (Directory . Map.fromList) $ replicateM n $ do
          nm <- arbName
          f <- oneof [arbFile, arbDirectory (n `div` 2)]
          return (nm,f)
