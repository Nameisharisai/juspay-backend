{-# LANGUAGE OverloadedStrings #-}

module Router where

import Data.Text (Text, pack, unpack)
import Database.Redis (Connection, connect, defaultConnectInfo, runRedis, get, setex, incr, expire)
import Control.Monad (when, guard, filterM)
import Control.Monad.IO.Class (liftIO)
import Control.Exception (try, SomeException)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.ByteString.Char8 as BS (pack, unpack)
import Data.Time (getCurrentTime, diffUTCTime, addUTCTime)
import Data.List (sortBy)
import Data.Ord (comparing)
import Errors (AppError(..))
import Config (Config(..))

data ProcessorConfig = ProcessorConfig
  { pcName :: Text
  , pcMinAmount :: Int
  , pcMaxAmount :: Int
  , pcPriority :: Int
  , pcHealthCheckUrl :: Text
  , pcEnabled :: Bool
  } deriving (Show, Eq)

data ProcessorHealth = ProcessorHealth
  { phProcessor :: Text
  , phIsHealthy :: Bool
  , phLastCheck :: Text
  , phResponseTime :: Int
  } deriving (Show, Eq)

availableProcessors :: [ProcessorConfig]
availableProcessors = 
  [ ProcessorConfig "instamojo" 1 1000000 1 "https://api.instamojo.com/health" True
  , ProcessorConfig "razorpay" 100 1000000 3 "https://api.razorpay.com/health" False
  ]

selectProcessor :: Config -> Int -> Text -> IO (Either AppError ProcessorConfig)
selectProcessor config amount country = do
  result <- try $ do
    redisConn <- connect defaultConnectInfo
    runRedis redisConn $ do
      let eligibleProcessors = filter (\p -> pcEnabled p && 
                                           amount >= pcMinAmount p && 
                                           amount <= pcMaxAmount p) 
                                     availableProcessors
      
      if null eligibleProcessors
        then return $ Left $ ValidationError "No processors available for this amount"
        else do
          healthyProcessors <- filterM (checkProcessorHealth redisConn) eligibleProcessors
          case healthyProcessors of
            [] -> return $ Right $ head eligibleProcessors
            _ -> do
              selected <- selectBestProcessor redisConn healthyProcessors
              return $ Right selected
  
  case result of
    Right processor -> return processor
    Left (e :: SomeException) -> return $ Left $ InternalError $ 
      "Processor selection failed: " <> pack (show e)

checkProcessorHealth :: Connection -> ProcessorConfig -> IO Bool
checkProcessorHealth redisConn processor = do
  result <- runRedis redisConn $ do
    let healthKey = BS.pack $ "health:" <> unpack (pcName processor)
    get healthKey
  case result of
    Right (Just "healthy") -> return True
    _ -> return False

selectBestProcessor :: Connection -> [ProcessorConfig] -> IO ProcessorConfig
selectBestProcessor redisConn processors = do
  loads <- mapM (getProcessorLoad redisConn) processors
  let processorsWithLoad = zip processors loads
      sortedProcessors = sortBy (comparing (\(p, l) -> (pcPriority p, l))) processorsWithLoad
  return $ fst $ head sortedProcessors

getProcessorLoad :: Connection -> ProcessorConfig -> IO Int
getProcessorLoad redisConn processor = do
  result <- runRedis redisConn $ do
    let loadKey = BS.pack $ "load:" <> unpack (pcName processor)
    get loadKey
  case result of
    Right (Just loadStr) -> return $ read $ BS.unpack loadStr
    _ -> return 0

updateProcessorHealth :: Config -> Text -> Bool -> Int -> IO (Either AppError ())
updateProcessorHealth config processorName isHealthy responseTime = do
  result <- try $ do
    redisConn <- connect defaultConnectInfo
    runRedis redisConn $ do
      let healthKey = BS.pack $ "health:" <> unpack processorName
          healthValue = if isHealthy then "healthy" else "unhealthy"
      _ <- setex healthKey 300 (BS.pack healthValue)
      return ()
  
  case result of
    Right _ -> return $ Right ()
    Left (e :: SomeException) -> return $ Left $ InternalError $ 
      "Failed to update processor health: " <> pack (show e)

recordProcessorUsage :: Config -> Text -> IO (Either AppError ())
recordProcessorUsage config processorName = do
  result <- try $ do
    redisConn <- connect defaultConnectInfo
    runRedis redisConn $ do
      let loadKey = BS.pack $ "load:" <> unpack processorName
      _ <- incr loadKey
      _ <- expire loadKey 60
      return ()
  
  case result of
    Right _ -> return $ Right ()
    Left (e :: SomeException) -> return $ Left $ InternalError $ 
      "Failed to record processor usage: " <> pack (show e)

data ProcessorStats = ProcessorStats
  { psProcessor :: Text
  , psHealthy :: Bool
  , psCurrentLoad :: Int
  , psLastUsed :: Text
  } deriving (Show, Eq)

getProcessorStats :: Config -> IO (Either AppError [ProcessorStats])
getProcessorStats config = do
  result <- try $ do
    redisConn <- connect defaultConnectInfo
    stats <- mapM (getProcessorStat redisConn) availableProcessors
    return stats
  
  case result of
    Right stats -> return $ Right stats
    Left (e :: SomeException) -> return $ Left $ InternalError $ 
      "Failed to get processor stats: " <> pack (show e)

getProcessorStat :: Connection -> ProcessorConfig -> IO ProcessorStats
getProcessorStat redisConn processor = do
  isHealthy <- checkProcessorHealth redisConn processor
  currentLoad <- getProcessorLoad redisConn processor
  return $ ProcessorStats
    { psProcessor = pcName processor
    , psHealthy = isHealthy
    , psCurrentLoad = currentLoad
    , psLastUsed = "unknown"
    }
