{-# LANGUAGE DeriveGeneric #-}
module Shadowsocks.Util where

import           Control.Monad (liftM)
import           Data.Aeson (decode', FromJSON)
import qualified Data.ByteString.Lazy as L
import           GHC.Generics (Generic)

data SSConfig = SSConfig
    { server        :: String
    , server_port   :: Int
    , local_port    :: Int
    , password      :: String
    , timeout       :: Int
    , method        :: String
    } deriving (Show, Generic)

instance FromJSON SSConfig

readConfig :: FilePath -> IO (Maybe SSConfig)
readConfig fp = liftM decode' $ L.readFile fp
