module Shadowsocks.Encrypt where

import Crypto.Hash.MD5 (hash)
import Data.Binary.Get (runGet, getWord64le)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as L
import Data.Word (Word8, Word64)

getTable :: ByteString -> [Word8]
getTable key = do
    let s = L.fromStrict $ hash key
        a = runGet getWord64le s
        table = [0..255]

    map fromIntegral $ sortTable 1 a table

sortTable :: Word64 -> Word64 -> [Word64] -> [Word64]
sortTable 1024 _ table = table
sortTable i a table = sortTable (i+1) a $ cmpsort table cmp
  where
    cmp x y = (a `mod` (x + i)) < (a `mod` (y + i)) 

cmpsort :: [Word64] -> (Word64 -> Word64 -> Bool) -> [Word64]
cmpsort [] _ = []
cmpsort (x:xs) cmp = 
    let left = [a | a <- xs, cmp a x]
        right = [a | a <- xs, not $ cmp a x]
     in cmpsort left cmp ++ [x] ++ cmpsort right cmp
