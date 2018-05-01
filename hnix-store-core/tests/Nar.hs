{-# LANGUAGE OverloadedStrings #-}

module Nar where

import Data.Maybe (isJust)
import Data.Binary.Put (Put(..), runPut)
import Data.Binary.Get (Get(..), runGet)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Set as Set
import Test.Tasty.Hspec

import System.Nix.Nar
import System.Nix.Path

sampleRegular :: FileSystemObject
sampleRegular = Regular NonExecutable "hi"
  -- "#include <stdio.h>\n\nint main(int argc, char *argv[]){ exit 0; }"

sampleExecutable :: FileSystemObject
sampleExecutable = Regular Executable
  "#!/bin/bash\n\ngcc -o hello hello.c"

sampleSymLink :: FileSystemObject
sampleSymLink = SymLink "hello.c"

sampleDirectory :: FileSystemObject
sampleDirectory = Directory $ Set.fromList
  [(PathName "hello.c", sampleRegular)
  ,(PathName "build.sh", sampleExecutable)
  ,(PathName "hi.c", sampleSymLink)
  ]

narBytes :: Nar -> BSL.ByteString
narBytes = runPut . putNar

spec_roundtrip :: Spec
spec_roundtrip = do

  let roundTrip n = runGet getNar (runPut $ putNar n) `shouldBe` n

  describe "parser-roundtrip" $ do
    it "roundtrips regular" $ do
      roundTrip (Nar sampleRegular)

    it "roundtrips executable" $ do
      roundTrip (Nar sampleExecutable)

    it "roundtrips symlink" $ do
      roundTrip (Nar sampleSymLink)

    it "roundtrips directory" $ do
      roundTrip (Nar sampleDirectory)
