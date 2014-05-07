{-# LANGUAGE OverloadedStrings #-}
module Shadowsocks.Encrypt
  ( getEncDec
  , iv_len
  ) where

import           Control.Concurrent.MVar ( newEmptyMVar, isEmptyMVar
                                         , putMVar, readMVar)
import           Crypto.Hash.MD5 (hash)
import           Data.Binary.Get (runGet, getWord64le)
import           Data.ByteString (ByteString)
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as L
import qualified Data.HashMap.Strict as HM
import           Data.IntMap.Strict (fromList, (!))
import           Data.List (sortBy)
import           Data.Maybe (fromJust)
import           Data.Monoid ((<>))
import           Data.Word (Word8, Word64)
import           OpenSSL (withOpenSSL)
import           OpenSSL.EVP.Cipher
import           OpenSSL.EVP.Internal
import           OpenSSL.Random (randBytes)


method_supported :: HM.HashMap String (Int, Int)
method_supported = HM.fromList
    [ ("aes-128-cfb", (16, 16))
    , ("aes-192-cfb", (24, 16))
    , ("aes-256-cfb", (32, 16))
    , ("bf-cfb", (16, 8))
    , ("camellia-128-cfb", (16, 16))
    , ("camellia-192-cfb", (24, 16))
    , ("camellia-256-cfb", (32, 16))
    , ("cast5-cfb", (16, 8))
    , ("des-cfb", (8, 8))
    , ("idea-cfb", (16, 8))
    , ("rc2-cfb", (16, 8))
    , ("rc4", (16, 0))
    , ("seed-cfb", (16, 16))
    ]

iv_len :: String -> Int
iv_len method = m1
  where
    (_, m1) = method_supported HM.! method

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

getSSLEncDec :: String -> ByteString
             -> IO (ByteString -> IO ByteString, ByteString -> IO ByteString)
getSSLEncDec method password = do
    let (m0, m1) = fromJust $ HM.lookup method method_supported
    random_iv <- withOpenSSL $ randBytes 32
    let cipher_iv = S.take m1 random_iv
    let (key, _) = evpBytesToKey password m0 m1
    cipherCtx <- newEmptyMVar
    decipherCtx <- newEmptyMVar

    cipherMethod <- fmap fromJust $ withOpenSSL $ getCipherByName method
    ctx <- cipherInit cipherMethod (C.unpack key) (C.unpack cipher_iv) Encrypt
    let
        encrypt "" = return ""
        encrypt buf = do
            empty <- isEmptyMVar cipherCtx
            if empty
                then do
                    putMVar cipherCtx ()
                    ciphered <- withOpenSSL $ cipherUpdateBS ctx buf
                    return $ cipher_iv <> ciphered
                else withOpenSSL $ cipherUpdateBS ctx buf
        decrypt "" = return ""
        decrypt buf = do
            empty <- isEmptyMVar decipherCtx
            if empty
                then do
                    let decipher_iv = S.take m1 buf
                    dctx <- cipherInit cipherMethod
                                       (C.unpack key)
                                       (C.unpack decipher_iv)
                                       Decrypt
                    putMVar decipherCtx dctx
                    if S.null (S.drop m1 buf)
                        then return ""
                        else withOpenSSL $ cipherUpdateBS dctx (S.drop m1 buf)
                else do
                    dctx <- readMVar decipherCtx
                    withOpenSSL $ cipherUpdateBS dctx buf

    return (encrypt, decrypt)

getTableEncDec :: ByteString
          -> IO (ByteString -> IO ByteString, ByteString -> IO ByteString)
getTableEncDec key = return (encrypt, decrypt)
  where
    table = getTable key
    encryptTable = fromList $ zip [0..255] table
    decryptTable = fromList $ zip (map fromIntegral table) [0..255]
    encrypt :: ByteString -> IO ByteString
    encrypt buf = return $
        S.pack $ map (\b -> encryptTable ! fromIntegral b) $ S.unpack buf
    decrypt :: ByteString -> IO ByteString
    decrypt buf = return $
        S.pack $ map (\b -> decryptTable ! fromIntegral b) $ S.unpack buf

getEncDec :: String -> String
          -> IO (ByteString -> IO ByteString, ByteString -> IO ByteString)
getEncDec "table" key = getTableEncDec $ C.pack key
getEncDec method key  = getSSLEncDec method $ C.pack key
