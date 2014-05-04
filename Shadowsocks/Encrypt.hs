{-# LANGUAGE OverloadedStrings #-}
module Shadowsocks.Encrypt where

import Control.Concurrent.MVar (MVar, newEmptyMVar, isEmptyMVar, putMVar, readMVar)
import Crypto.Hash.MD5 (hash)
import Data.Binary.Get (runGet, getWord64le)
import Data.ByteString (ByteString)
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as L
import Data.IntMap.Strict (fromList, (!))
import Data.List (sortBy)
import Data.Maybe (fromJust)
import Data.Monoid ((<>))
import Data.Word (Word8, Word64)
import OpenSSL (withOpenSSL)
import OpenSSL.EVP.Cipher
import OpenSSL.Random (randBytes)
import System.IO.Unsafe (unsafePerformIO)
import Data.Char

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

evpBytesToKey :: ByteString -> Int -> Int -> (ByteString, ByteString)
evpBytesToKey password keyLen ivLen =
    let ms' = S.concat $ ms 0 []
        key = S.take keyLen ms'
        iv  = S.take ivLen $ S.drop keyLen ms'
     in (key, iv)
  where
    ms :: Int -> [ByteString] -> [ByteString]
    ms 0 _ = ms 1 [hash password]
    ms i m
        | S.length (S.concat m) < keyLen + ivLen =
            ms (i+1) (m ++ [hash (last m <> password)])
        | otherwise = m

getCipher :: ByteString -> CryptoMode -> IO (ByteString -> IO ByteString)
getCipher iv mode = do
    let (key, iv_) = evpBytesToKey "abc" 16 8
    method <- fmap fromJust $ withOpenSSL $ getCipherByName "bf-cfb"
    return $ cipherBS method (C.unpack key) (C.unpack iv) mode

cipherMVar :: MVar ()
cipherMVar = unsafePerformIO newEmptyMVar
decipherMVar :: MVar (ByteString -> IO ByteString)
decipherMVar = unsafePerformIO newEmptyMVar

getEncDec :: ByteString
          -> IO (ByteString -> IO ByteString, ByteString -> IO ByteString)
getEncDec iv = do

    random_iv <- if S.null iv then withOpenSSL $ randBytes 32
                              else return iv
    let cipher_iv = S.take 8 random_iv

    myCipher <- getCipher random_iv Encrypt
    let
        encrypt "" = return ""
        encrypt buf = do
            empty <- isEmptyMVar cipherMVar
            if empty
                then do
                    putMVar cipherMVar ()
                    ciphered <- withOpenSSL $ myCipher buf
                    print cipher_iv
                    print ciphered
                    return $ cipher_iv <> ciphered
                else
                    withOpenSSL (myCipher buf)
        decrypt "" = return ""
        decrypt buf = do
            empty <- isEmptyMVar decipherMVar
            if empty
                then do
                    print $ S.length buf
                    let decipher_iv = S.take 8 buf
                    myDecipher <- getCipher decipher_iv Decrypt
                    putMVar decipherMVar myDecipher
                    if S.null (S.drop 8 buf)
                        then return ""
                        else withOpenSSL $ myDecipher (S.drop 8 buf)
                else do
                    myDecipher <- readMVar decipherMVar
                    withOpenSSL $ myDecipher buf

    return (encrypt, decrypt)

getTableEncDec :: ByteString
          -> IO (ByteString -> IO ByteString, ByteString -> IO ByteString)
getTableEncDec _ = do
    let table = getTable "abc"
        encryptTable = fromList $ zip [0..255] table
        decryptTable = fromList $ zip (map fromIntegral table) [0..255]
        encrypt :: ByteString -> IO ByteString
        encrypt buf = return $ S.pack $ map (\b -> encryptTable ! fromIntegral b) $ S.unpack buf
        decrypt :: ByteString -> IO ByteString
        decrypt buf = return $ S.pack $ map (\b -> decryptTable ! fromIntegral b) $ S.unpack buf

    return (encrypt, decrypt)
