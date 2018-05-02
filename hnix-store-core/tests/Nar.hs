{-# LANGUAGE OverloadedStrings #-}

module Nar where

import           Data.Binary.Get             (Get (..), runGet)
import           Data.Binary.Put             (Put (..), runPut)
import qualified Data.ByteString.Base64.Lazy as B64
import qualified Data.ByteString.Lazy        as BSL
import qualified Data.Map                    as Map
import           Data.Maybe                  (isJust)
import           Test.Tasty.Hspec

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


  describe "matches-nix-store" $ do
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
  [(PathName "hello.c", sampleRegular')
  ,(PathName "build.sh", sampleExecutable)
  ,(PathName "hi.c", sampleSymLink)
  ]

-- | A deeper directory tree with crossing links
sampleDirectory' :: FileSystemObject
sampleDirectory' = Directory $ Map.fromList [

    (PathName "foo", Directory $ Map.fromList [
        (PathName "foo.txt", Regular NonExecutable "foo text")
      , (PathName "tobar"  , SymLink "../bar/bar.txt")
      ])

  , (PathName "bar", Directory $ Map.fromList [
        (PathName "bar.txt", Regular NonExecutable "bar text")
      , (PathName "tofoo"  , SymLink "../foo/foo.txt")
      ])
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
