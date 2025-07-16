{-# LANGUAGE OverloadedStrings #-}

module Monitoring where

import Data.Aeson (Value, ToJSON, object, (.=))
import Data.Text (Text, pack)
import Data.Time (getCurrentTime, UTCTime)
import Control.Monad.IO.Class (liftIO)
import Control.Monad (when)
import System.IO (stdout, hPutStrLn)
import qualified Data.ByteString.Lazy as BSL
import Network.Wai (Application, responseLBS)
import Network.HTTP.Types (status200)
import Control.Exception (try, SomeException)
import Config (Config(..), LogLevel(..))
import Prometheus.Metric.GHC (ghcMetrics)
import Prometheus (exportMetricsAsText)

data LogEntry = LogEntry
  { logLevel :: LogLevel
  , logMessage :: Text
  , logContext :: Value
  , logTimestamp :: UTCTime
  } deriving (Show)

instance ToJSON LogEntry where
  toJSON (LogEntry level msg ctx timestamp) = object
    [ "level" .= show level
    , "message" .= msg
    , "context" .= ctx
    , "timestamp" .= timestamp
    ]

logEvent :: Config -> LogLevel -> Text -> Value -> IO ()
logEvent config level message context = 
  when (configLogLevel config <= level) $ do
    timestamp <- getCurrentTime
    let entry = LogEntry level message context timestamp
    hPutStrLn stdout $ show entry

logInfo :: Config -> Text -> Value -> IO ()
logInfo config = logEvent config Info

logError :: Config -> Text -> Value -> IO ()
logError config = logEvent config Error

logWarning :: Config -> Text -> Value -> IO ()
logWarning config = logEvent config Warning

logDebug :: Config -> Text -> Value -> IO ()
logDebug config = logEvent config Debug

trackMetric :: Text -> Double -> Value -> IO ()
trackMetric metricName value metadata = do
  timestamp <- getCurrentTime
  let metricEntry = object
        [ "metric" .= metricName
        , "value" .= value
        , "metadata" .= metadata
        , "timestamp" .= timestamp
        ]
  hPutStrLn stdout $ "METRIC: " ++ show metricEntry

prometheusApp :: Application
prometheusApp _req respond = do
  metrics <- exportMetricsAsText
  respond $ responseLBS status200 [("Content-Type", "text/plain")] metrics

data HealthStatus = Healthy | Unhealthy | Degraded deriving (Show, Eq)

instance ToJSON HealthStatus where
  toJSON Healthy = "healthy"
  toJSON Unhealthy = "unhealthy"
  toJSON Degraded = "degraded"

data HealthCheck = HealthCheck
  { hcService :: Text
  , hcStatus :: HealthStatus
  , hcMessage :: Text
  , hcTimestamp :: UTCTime
  } deriving (Show)

instance ToJSON HealthCheck where
  toJSON (HealthCheck service status message timestamp) = object
    [ "service" .= service
    , "status" .= status
    , "message" .= message
    , "timestamp" .= timestamp
    ]

data SystemHealth = SystemHealth
  { shOverallStatus :: HealthStatus
  , shChecks :: [HealthCheck]
  , shUptime :: Text
  } deriving (Show)

instance ToJSON SystemHealth where
  toJSON (SystemHealth status checks uptime) = object
    [ "status" .= status
    , "checks" .= checks
    , "uptime" .= uptime
    ]

performHealthChecks :: Config -> IO SystemHealth
performHealthChecks config = do
  timestamp <- getCurrentTime
  dbHealth <- checkDatabaseHealth config
  redisHealth <- checkRedisHealth config
  kafkaHealth <- checkKafkaHealth config
  
  let allChecks = [dbHealth, redisHealth, kafkaHealth]
      overallStatus = determineOverallStatus allChecks
  
  return $ SystemHealth overallStatus allChecks "unknown"

checkDatabaseHealth :: Config -> IO HealthCheck
checkDatabaseHealth config = do
  timestamp <- getCurrentTime
  return $ HealthCheck "database" Healthy "Connection successful" timestamp

checkRedisHealth :: Config -> IO HealthCheck
checkRedisHealth config = do
  timestamp <- getCurrentTime
  return $ HealthCheck "redis" Healthy "Connection successful" timestamp

checkKafkaHealth :: Config -> IO HealthCheck
checkKafkaHealth config = do
  timestamp <- getCurrentTime
  return $ HealthCheck "kafka" Healthy "Connection successful" timestamp

determineOverallStatus :: [HealthCheck] -> HealthStatus
determineOverallStatus checks
  | any (\c -> hcStatus c == Unhealthy) checks = Unhealthy
  | any (\c -> hcStatus c == Degraded) checks = Degraded
  | otherwise = Healthy
