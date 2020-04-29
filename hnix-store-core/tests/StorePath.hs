{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module StorePath where

import qualified Data.ByteString.Char8            as BSC
import qualified Data.Attoparsec.ByteString.Char8 as P

import           Test.Tasty.QuickCheck

import           System.Nix.Internal.StorePath
import           Arbitrary

-- | Test that Nix(OS) like paths roundtrip
prop_storePathRoundtrip (_ :: NixLike) = \(NixLike x) ->
  (parsePath "/nix/store" $ storePathToRawFilePath x) === Right x

-- | Test that any `StorePath` roundtrips
prop_storePathRoundtrip' x =
  (parsePath (storePathRoot x) $ storePathToRawFilePath x) === Right x

prop_storePathRoundtripParser (_ :: NixLike) = \(NixLike x) ->
  (P.parseOnly (pathParser (storePathRoot x))
    $ storePathToRawFilePath x) === Right x

prop_storePathRoundtripParser' x =
  (P.parseOnly (pathParser (storePathRoot x))
    $ storePathToRawFilePath x) === Right x
