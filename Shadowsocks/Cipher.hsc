-- Shamelessly taken from https://github.com/phonohawk/HsOpenSSL
-- With ByteString version of cipherInit

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Shadowsocks.Cipher
  ( CryptoMode (..)
  , cipherInit
  , cipherUpdateBS
  , getCipherByName
  ) where

#include <openssl/evp.h>

import           Control.Applicative ((<$>))
import           Control.Exception (mask_)
import           Control.Monad (void)
import qualified Data.ByteString.Internal as B8
import           Data.ByteString (useAsCStringLen)
import           Foreign.C
import           Foreign.C.Types
import           Foreign.ForeignPtr
import           Foreign.Marshal.Alloc (alloca)
import           Foreign.Ptr
import           Foreign.Storable (Storable(..))

foreign import ccall unsafe "EVP_CIPHER_CTX_init"
    _cipher_ctx_init :: Ptr EVP_CIPHER_CTX -> IO ()

foreign import ccall unsafe "&EVP_CIPHER_CTX_cleanup"
    _cipher_ctx_cleanup :: FunPtr (Ptr EVP_CIPHER_CTX -> IO ())

foreign import ccall unsafe "EVP_CIPHER_CTX_block_size"
    _cipher_ctx_block_size :: Ptr EVP_CIPHER_CTX -> CInt

foreign import ccall unsafe "EVP_get_cipherbyname"
    _get_cipherbyname :: CString -> IO (Ptr EVP_CIPHER)

foreign import ccall unsafe "EVP_CipherInit"
     _CipherInit :: Ptr EVP_CIPHER_CTX
                 -> Ptr EVP_CIPHER -> CString -> CString -> CInt -> IO CInt

foreign import ccall unsafe "EVP_CipherUpdate"
  _CipherUpdate :: Ptr EVP_CIPHER_CTX -> Ptr CChar -> Ptr CInt
                -> Ptr CChar -> CInt -> IO CInt

newtype Cipher    = Cipher (Ptr EVP_CIPHER)
data    EVP_CIPHER

newtype CipherCtx = CipherCtx (ForeignPtr EVP_CIPHER_CTX)
data    EVP_CIPHER_CTX

newCipherCtx :: IO CipherCtx
newCipherCtx = do
    ctx <- mallocForeignPtrBytes (#size EVP_CIPHER_CTX)
    mask_ $ do
        withForeignPtr ctx _cipher_ctx_init
        addForeignPtrFinalizer _cipher_ctx_cleanup ctx
    return $ CipherCtx ctx

withCipherCtxPtr :: CipherCtx -> (Ptr EVP_CIPHER_CTX -> IO a) -> IO a
withCipherCtxPtr (CipherCtx ctx) = withForeignPtr ctx

-- |@'getCipherByName' name@ returns a symmetric cipher algorithm
-- whose name is @name@. If no algorithms are found, the result is
-- @Nothing@.
getCipherByName :: String -> IO (Maybe Cipher)
getCipherByName name
    = withCString name $ \ namePtr ->
      do ptr <- _get_cipherbyname namePtr
         return $ if ptr == nullPtr then Nothing else Just $ Cipher ptr

data CryptoMode = Encrypt | Decrypt

cryptoModeToInt :: CryptoMode -> CInt
cryptoModeToInt Encrypt = 1
cryptoModeToInt Decrypt = 0

cipherInit :: Cipher
           -> B8.ByteString -> B8.ByteString -> CryptoMode -> IO CipherCtx
cipherInit (Cipher c) key iv mode
    = do ctx <- newCipherCtx
         withCipherCtxPtr ctx $ \ ctxPtr ->
             useAsCStringLen key $ \ (keyPtr, _) ->
                 useAsCStringLen iv $ \ (ivPtr, _) ->
                     _CipherInit ctxPtr c keyPtr ivPtr (cryptoModeToInt mode)
                          >>= failIf_ (/= 1)
         return ctx

cipherUpdateBS :: CipherCtx -> B8.ByteString -> IO B8.ByteString
cipherUpdateBS ctx inBS =
  withCipherCtxPtr ctx $ \ctxPtr ->
    useAsCStringLen inBS $ \(inBuf, inLen) ->
      let len = inLen + fromIntegral (_cipher_ctx_block_size ctxPtr) - 1 in
        B8.createAndTrim len $ \outBuf ->
          alloca $ \outLenPtr ->
            _CipherUpdate ctxPtr (castPtr outBuf) outLenPtr inBuf
                          (fromIntegral inLen)
              >>= failIf (/= 1)
              >>  fromIntegral <$> peek outLenPtr

failIf :: (a -> Bool) -> a -> IO a
failIf f a
    | f a       = error "error"
    | otherwise = return a

failIf_ :: (a -> Bool) -> a -> IO ()
failIf_ f a = void $ failIf f a
