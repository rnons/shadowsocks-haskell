{-# LANGUAGE DeriveGeneric #-}
module Shadowsocks.Util
  ( SSConfig (..)
  , SSConfig' (..)
  , nullConfig
  , readConfig
  , configOptions
  ) where

import           Control.Monad (liftM)
import           Data.Aeson (decode', FromJSON)
import qualified Data.ByteString.Lazy as L
import           GHC.Generics (Generic)
import           Options.Applicative
import           Options.Applicative.Types (ReadM(ReadM))

data SSConfig = SSConfig
    { server        :: String
    , server_port   :: Int
    , local_port    :: Int
    , password      :: String
    , timeout       :: Int
    , method        :: String
    } deriving (Show, Generic)

instance FromJSON SSConfig

data SSConfig' = SSConfig'
    { _config        :: Maybe String
    , _server        :: Maybe String
    , _server_port   :: Maybe Int
    , _local_port    :: Maybe Int
    , _password      :: Maybe String
    , _method        :: Maybe String
    } deriving (Show, Generic)

nullConfig :: SSConfig
nullConfig = SSConfig "" 0 0 "" 0 ""

readConfig :: FilePath -> IO (Maybe SSConfig)
readConfig fp = liftM decode' $ L.readFile fp

optStr :: Mod OptionFields (Maybe String) -> Parser (Maybe String)
optStr m =
    nullOption $ value Nothing <> reader (success . str) <> m
  where
    success = ReadM . Right

maybeOpt :: Read a => Mod OptionFields (Maybe a) -> Parser (Maybe a)
maybeOpt m =
    nullOption $ value Nothing <> reader (success . auto) <> m
  where
    success = ReadM . Right

configOptions :: Parser SSConfig'
configOptions = SSConfig'
    <$> optStr (long "config" <> short 'c' <> metavar "CONFIG"
               <> help "path to config file")
    <*> optStr (long "server" <> short 's' <> metavar "ADDR"
               <> help "server address")
    <*> maybeOpt (long "server_port" <> short 'p' <> metavar "PORT"
                 <> help "server port")
    <*> maybeOpt (long "local_port" <> short 'l' <> metavar "PORT"
                 <> help "local port")
    <*> optStr (long "password" <> short 'k' <> metavar "PASSWORD"
               <> help "password")
    <*> optStr (long "method" <> short 'm' <> metavar "METHOD"
               <> help "encryption method, for example, aes-256-cfb")
