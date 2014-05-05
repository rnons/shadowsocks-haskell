#!/usr/bin/env runhaskell
{-# LANGUAGE OverloadedStrings #-}
import Crypto.Hash.MD5 (hash)
import Data.Binary.Get (runGet, getWord64le)
import Data.ByteString (ByteString)
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import Data.List (sortBy)
import Data.IntMap.Strict (fromList, (!))
import Data.Word (Word8, Word64)


aPoem :: ByteString
aPoem = "First they came for the Socialists, and I did not speak out-- Because I was not a Socialist.  Then they came for the Trade Unionists, and I did not speak out-- Because I was not a Trade Unionist. Then they came for the Jews, and I did not speak out-- Because I was not a Jew. Then they came for me--and there was no one left to speak for me."

getTable :: ByteString -> [Word8]
getTable key = do
    let s = L.fromStrict $ hash key
        a = runGet getWord64le s
        table = [0..255]

    map fromIntegral $ sortTable 1 a table

sortTable :: Word64 -> Word64 -> [Word64] -> [Word64]
sortTable 1024 _ table = table
sortTable i a table = sortTable (i+1) a $ sortBy cmp table
  where
    cmp x y = compare (a `mod` (x + i)) (a `mod` (y + i))

main :: IO ()
main = do
    encrypted <- encrypt aPoem
    decrypted <- decrypt encrypted
    print $ aPoem == decrypted
  where
    table = getTable "Don't panic!"
    encryptTable = fromList $ zip [0..255] table
    decryptTable = fromList $ zip (map fromIntegral table) [0..255]
    encrypt :: ByteString -> IO ByteString
    encrypt buf = return $
        S.pack $ map (\b -> encryptTable ! fromIntegral b) $ S.unpack buf
    decrypt :: ByteString -> IO ByteString
    decrypt buf = return $
        S.pack $ map (\b -> decryptTable ! fromIntegral b) $ S.unpack buf

