{-|
Description : Trunctions of cryptographic hashes.
Maintainer  : Shea Levy <shea@shealevy.com>
-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE CPP #-}

module Crypto.Hash.Truncated where

import           Data.Bits
import           Crypto.Hash.SHA256 (hash)
import           Data.Proxy (Proxy(..))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import           Data.Word (Word8)
import           Debug.Trace



newtype PathHash = PathHash { getPathHash :: String }
  deriving (Eq, Show)

toBase32 :: BS.ByteString -> PathHash
toBase32 digest = PathHash $ map (nthChar digest) [0.. pred base32Len]


-- 000000001111111122222222333333334444444455555555  -- BS index
-- |       |       |       |       |       |
-- 001011001001101111100101111011101010100010100000  -- BS to transcribe
-- |    |    |    |    |    |    |    |    |    |
-- 000001111122222333334444455555666667777788888999
-- (0,0)(0,5)(1,2)(1,7)(2,4)                         -- (i0,o0)
--  0    2    0    4                                 -- clip
--  0    2    0    4    1                            -- overhang
--  3    0    1    0    0                            -- underhang

-- Example
-- h = hash "Hello!"
-- > [51,77,1,111,117,92,214,220,88,197,58,134,225,131,136,47,142,193,79,82,251,5,52,88,135,200,165,237,212,44,135,183]
-- |51     |77     |1      |111    |117
-- 0011001101001101000000010110111101110101
-- |6   |13  |3   |32  |2   |    |    |
-- 6d6h2vvmbkbdqn657a3f30w85y7c2ks
-- 6d6h2vvmbkbdqn657a3f30w85y7c2ks

nthChar :: BS.ByteString -> Int -> Char
nthChar bs n =
  let (i0,o0) = (5 * n) `divMod` 8
      clip    = max 0 ((o0 + 5) - 8)
      underhang = if clip == 0 then 3 - o0 else 0
      l0      = 5 - clip
      ci = bs `BS.index` i0
      cj = if i0 + 2 >= BS.length bs then 0 else bs `BS.index` (i0 + 1)
      bytesi = ci `shiftL` o0 `shiftR` o0 `shiftL` clip `shiftR` underhang
      bytesj = cj `shiftR` (8 - clip)
  in  charLookup (bytesi .|. bytesj)

nthCharDebug :: BS.ByteString -> Int -> Char
nthCharDebug bs n =
  let (i0,o0) = tp "(i0,o0)" $ (5 * n) `divMod` 8
      clip    = tp "clip" $ max 0 ((o0 + 5) - 8)
      underhang = tp "underhang" $ if clip == 0 then 3 - o0 else 0
      l0      = tp "l0" $ 5 - clip
      ci = tp "ci" $ bs `BS.index` i0
      cj = tp "cj" $ if i0 + 2 >= BS.length bs then 0 else bs `BS.index` (i0 + 1)
      bytesi = tp "bytesi" $ ci `shiftL` o0 `shiftR` o0 `shiftL` clip `shiftR` underhang
      bytesj = tp "bytesj" $ cj `shiftR` (8 - clip)
      tp s a = trace (s ++ " " ++ show a) a
  in  charLookup (bytesi .|. bytesj)

charLookup :: Word8 -> Char
charLookup i =
  if i < 32
  then "0123456789abcdfghijklmnpqrsvwxyz" `BSC.index` fromIntegral i
  else error $ "Error in base32 encoding logic gave bad index: " ++ show i

-- 32 characters
base32Len :: Int
base32Len = (32 * 8 `div` 5) - 20
