{-# LANGUAGE OverloadedStrings #-}

module Fraud where

import Data.Text (Text, pack, unpack)
import Database.Persist
import Database.Redis (Connection, connect, defaultConnectInfo, runRedis, incr, get, expire, RedisCtx)
import Control.Monad.IO.Class (liftIO)
import Control.Exception (bracket, try, SomeException)
import Data.ByteString.Char8 as BS (pack)
import Control.Monad (when)
import Data.Time (getCurrentTime, UTCTime)
import Data.Maybe (fromMaybe)
import Errors (AppError(..))
import Config (Config(..))

data FraudResult = FraudResult 
  { isFraud :: Bool
  , reason :: Text
  , riskScore :: Double
  , fraudChecks :: [FraudCheck]
  } deriving (Show, Eq)

data FraudCheck = FraudCheck
  { checkName :: Text
  , checkPassed :: Bool
  , checkScore :: Double
  , checkDetails :: Text
  } deriving (Show, Eq)

checkFraud :: Config -> Int -> Text -> Text -> SqlPersistT IO (Either AppError FraudResult)
checkFraud config amount ip country = do
  result <- liftIO $ try $ do
    redisConn <- connect defaultConnectInfo
    runRedis redisConn $ do
      velocityResult <- checkVelocity ip
      let amountResult = checkAmount amount
      let geoResult = checkGeographic country
      ipResult <- checkIPReputation ip
      
      let allChecks = [velocityResult, amountResult, geoResult, ipResult]
          totalScore = sum $ map checkScore allChecks
          isFraudulent = totalScore > 50.0
          mainReason = if isFraudulent then getFraudReason allChecks else "Transaction approved"
      
      return $ FraudResult
        { isFraud = isFraudulent
        , reason = mainReason
        , riskScore = totalScore
        , fraudChecks = allChecks
        }
  
  case result of
    Right fraudResult -> return $ Right fraudResult
    Left (e :: SomeException) -> return $ Left $ InternalError $ 
      "Fraud detection failed: " <> pack (show e)

checkVelocity :: Text -> RedisCtx m f => m FraudCheck
checkVelocity ip = do
  let key = BS.pack $ "velocity:" <> unpack ip
  currentCount <- incr key
  _ <- expire key 60
  
  let score = case currentCount of
        Right count | count > 10 -> 30.0
                   | count > 5 -> 15.0
                   | otherwise -> 0.0
        _ -> 5.0
        
  return $ FraudCheck "velocity" (score < 20.0) score 
    ("Transaction count in last minute: " <> pack (show currentCount))

checkAmount :: Int -> FraudCheck
checkAmount amount = 
  let highRiskThreshold = 500000
      mediumRiskThreshold = 100000
      score = if amount > highRiskThreshold then 25.0
              else if amount > mediumRiskThreshold then 10.0
              else 0.0
  in FraudCheck "amount" (score < 20.0) score 
       ("Amount: " <> pack (show amount))

checkGeographic :: Text -> FraudCheck
checkGeographic country = 
  let highRiskCountries = ["XX", "YY"]
      score = if country `elem` highRiskCountries then 20.0 else 0.0
  in FraudCheck "geographic" (score < 15.0) score 
       ("Country: " <> country)

checkIPReputation :: Text -> RedisCtx m f => m FraudCheck
checkIPReputation ip = do
  let key = BS.pack $ "ip_reputation:" <> unpack ip
  reputation <- get key
  
  let score = case reputation of
        Right (Just _) -> 0.0
        _ -> 10.0
        
  return $ FraudCheck "ip_reputation" (score < 15.0) score 
    ("IP: " <> ip)

getFraudReason :: [FraudCheck] -> Text
getFraudReason checks = 
  case filter (not . checkPassed) checks of
    [] -> "No fraud detected"
    failedChecks -> "Failed checks: " <> pack (show $ map checkName failedChecks)
