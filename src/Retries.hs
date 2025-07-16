{-# LANGUAGE OverloadedStrings #-}

module Retries where

import Data.Aeson (Value, ToJSON(..), FromJSON, object, (.=), (.:), withObject)
import Data.Text (Text, pack)
import Data.Time (UTCTime, getCurrentTime, addUTCTime, diffUTCTime)
import Control.Monad.IO.Class (liftIO)
import Control.Monad (when, forever, forM_)
import Control.Concurrent (threadDelay)
import Control.Exception (try, SomeException)
import Data.ByteString.Char8 as BS (pack)
import Database.Persist
import Database.Redis (Connection, connect, defaultConnectInfo, runRedis, lpush, brpop)
import Config (Config(..))
import Errors (AppError(..))
import GHC.Generics (Generic)

data RetryJob = RetryJob
  { rjId :: Text
  , rjTransactionId :: Text
  , rjProcessor :: Text
  , rjPayload :: Value
  , rjAttempt :: Int
  , rjMaxAttempts :: Int
  , rjNextRetryAt :: UTCTime
  , rjLastError :: Text
  , rjCreatedAt :: UTCTime
  } deriving (Show, Generic)

instance ToJSON RetryJob
instance FromJSON RetryJob

data RetryResult = Success | Failed | ShouldRetry deriving (Show, Eq)

data BackoffConfig = BackoffConfig
  { bcInitialDelay :: Int
  , bcMaxDelay :: Int
  , bcMultiplier :: Double
  , bcMaxAttempts :: Int
  } deriving (Show)

defaultBackoffConfig :: BackoffConfig
defaultBackoffConfig = BackoffConfig
  { bcInitialDelay = 5
  , bcMaxDelay = 300
  , bcMultiplier = 2.0
  , bcMaxAttempts = 5
  }

queueRetryJob :: Config -> Text -> Text -> Value -> Int -> IO (Either AppError ())
queueRetryJob config transactionId processor payload attempt = do
  result <- try $ do
    now <- getCurrentTime
    let retryDelay = calculateBackoffDelay defaultBackoffConfig attempt
        nextRetryTime = addUTCTime (fromIntegral retryDelay) now
        retryJob = RetryJob
          { rjId = transactionId <> "-retry-" <> pack (show attempt)
          , rjTransactionId = transactionId
          , rjProcessor = processor
          , rjPayload = payload
          , rjAttempt = attempt
          , rjMaxAttempts = bcMaxAttempts defaultBackoffConfig
          , rjNextRetryAt = nextRetryTime
          , rjLastError = ""
          , rjCreatedAt = now
          }
    
    redisConn <- connect defaultConnectInfo
    runRedis redisConn $ do
      let queueKey = BS.pack "retry_queue"
          jobData = BS.pack $ show retryJob
      _ <- lpush queueKey [jobData]
      return ()
    
    return ()
  
  case result of
    Right _ -> return $ Right ()
    Left (e :: SomeException) -> return $ Left $ InternalError $ 
      "Failed to queue retry job: " <> pack (show e)

processRetryJob :: Config -> RetryJob -> IO RetryResult
processRetryJob config job = do
  now <- getCurrentTime
  let timeDiff = diffUTCTime now (rjNextRetryAt job)
  
  if timeDiff < 0
    then return ShouldRetry
    else do
      if rjAttempt job >= rjMaxAttempts job
        then do
          return Failed
        else do
          result <- try $ do
            return Success
          
          case result of
            Right Success -> do
              return Success
            Right Failed -> do
              return Failed
            Left (e :: SomeException) -> do
              let nextAttempt = rjAttempt job + 1
              if nextAttempt >= rjMaxAttempts job
                then return Failed
                else do
                  _ <- queueRetryJob config (rjTransactionId job) (rjProcessor job) 
                         (rjPayload job) nextAttempt
                  return ShouldRetry

calculateBackoffDelay :: BackoffConfig -> Int -> Int
calculateBackoffDelay config attempt = 
  let delay = fromIntegral (bcInitialDelay config) * (bcMultiplier config ** fromIntegral attempt)
      maxDelay = fromIntegral (bcMaxDelay config)
  in round $ min delay maxDelay

startRetryProcessor :: Config -> IO ()
startRetryProcessor config = forever $ do
  result <- try $ do
    redisConn <- connect defaultConnectInfo
    runRedis redisConn $ do
      let queueKey = BS.pack "retry_queue"
      jobResult <- brpop [queueKey] 10
      case jobResult of
        Right (Just (_, jobData)) -> do
          let jobStr = BS.unpack jobData
          case reads jobStr of
            [(job, "")] -> do
              liftIO $ do
                result <- processRetryJob config job
                case result of
                  Success -> return ()
                  Failed -> return ()
                  ShouldRetry -> return ()
            _ -> return ()
        _ -> return ()
  
  case result of
    Right _ -> return ()
    Left (e :: SomeException) -> return ()
  
  threadDelay (30 * 1000000)
