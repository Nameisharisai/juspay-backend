{-# LANGUAGE OverloadedStrings #-}

module Config where

import Data.Text (Text, pack)
import System.Environment (lookupEnv)
import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)

data Config = Config
  { configPort :: Int
  , configDatabaseUrl :: Text
  , configRedisUrl :: Text
  , configKafkaBootstrap :: Text
  , configInstamojoApiKey :: Text
  , configInstamojoAuthToken :: Text
  , configInstamojoBaseUrl :: Text
  , configLogLevel :: LogLevel
  , configMaxConnections :: Int
  } deriving (Show, Eq)

data LogLevel = Debug | Info | Warning | Error deriving (Show, Eq, Read)

loadConfig :: IO Config
loadConfig = do
  portStr <- lookupEnv "PORT"
  dbUrl <- lookupEnv "POSTGRES_URL"
  redisUrl <- lookupEnv "REDIS_URL"
  kafkaBootstrap <- lookupEnv "KAFKA_BOOTSTRAP"
  instamojoApiKey <- lookupEnv "INSTAMOJO_API_KEY"
  instamojoAuthToken <- lookupEnv "INSTAMOJO_AUTH_TOKEN"
  instamojoBaseUrl <- lookupEnv "INSTAMOJO_BASE_URL"
  logLevelStr <- lookupEnv "LOG_LEVEL"
  maxConnsStr <- lookupEnv "MAX_CONNECTIONS"

  let port = maybe 8080 id (portStr >>= readMaybe)
      logLevel = maybe Info id (logLevelStr >>= readMaybe)
      maxConns = maybe 10 id (maxConnsStr >>= readMaybe)

  return $ Config
    { configPort = port
    , configDatabaseUrl = pack $ fromMaybe "postgresql://juspay_user:password@localhost:5432/juspay" dbUrl
    , configRedisUrl = pack $ fromMaybe "redis://localhost:6379" redisUrl
    , configKafkaBootstrap = pack $ fromMaybe "localhost:9092" kafkaBootstrap
    , configInstamojoApiKey = pack $ fromMaybe "test_key" instamojoApiKey
    , configInstamojoAuthToken = pack $ fromMaybe "test_token" instamojoAuthToken
    , configInstamojoBaseUrl = pack $ fromMaybe "https://api.instamojo.com" instamojoBaseUrl
    , configLogLevel = logLevel
    , configMaxConnections = maxConns
    }

getConfig :: IO Config
getConfig = loadConfig
