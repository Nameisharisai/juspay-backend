{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}

module Models where

import Database.Persist.TH
import Database.Persist.Postgresql
import Data.Text (Text)
import Data.Aeson (Value, ToJSON(..), FromJSON, (.=), object)
import Data.Time (UTCTime, getCurrentTime)
import GHC.Generics (Generic)
import Control.Exception (try, SomeException)
import Control.Monad.IO.Class (liftIO)
import Errors (AppError(..))
import Config (Config(..))

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Transaction
    id Text
    amount Int
    currency Text
    status Text
    processor Text
    token Text Maybe
    ipAddress Text
    fraudFlag Bool default=False
    fraudScore Double default=0.0
    createdAt UTCTime default=CURRENT_TIMESTAMP
    updatedAt UTCTime default=CURRENT_TIMESTAMP
    Primary id
    deriving Show Eq Generic

AuditLog
    transactionId Text Maybe
    event Text
    details Value
    ipAddress Text Maybe
    userAgent Text Maybe
    createdAt UTCTime default=CURRENT_TIMESTAMP
    Foreign Transaction fk_transaction_id transactionId
    deriving Show Eq Generic
|]

instance ToJSON Transaction
instance FromJSON Transaction
instance ToJSON AuditLog  
instance FromJSON AuditLog
instance ToJSON PaymentMethod
instance FromJSON PaymentMethod
instance ToJSON ProcessorConfig
instance FromJSON ProcessorConfig
instance ToJSON WebhookDelivery
instance FromJSON WebhookDelivery
instance ToJSON FraudRule
instance FromJSON FraudRule

data TransactionStatus 
  = Pending 
  | Processing 
  | Success 
  | Failed 
  | Cancelled 
  | Refunded
  deriving (Show, Eq, Generic)

instance ToJSON TransactionStatus where
  toJSON Pending = "pending"
  toJSON Processing = "processing"
  toJSON Success = "success"
  toJSON Failed = "failed"
  toJSON Cancelled = "cancelled"
  toJSON Refunded = "refunded"

transactionStatusToText :: TransactionStatus -> Text
transactionStatusToText Pending = "pending"
transactionStatusToText Processing = "processing"
transactionStatusToText Success = "success"
transactionStatusToText Failed = "failed"
transactionStatusToText Cancelled = "cancelled"
transactionStatusToText Refunded = "refunded"

parseTransactionStatus :: Text -> Maybe TransactionStatus
parseTransactionStatus "pending" = Just Pending
parseTransactionStatus "processing" = Just Processing
parseTransactionStatus "success" = Just Success
parseTransactionStatus "failed" = Just Failed
parseTransactionStatus "cancelled" = Just Cancelled
parseTransactionStatus "refunded" = Just Refunded
parseTransactionStatus _ = Nothing

createConnectionPool :: Config -> IO (Either AppError ConnectionPool)
createConnectionPool config = do
  result <- try $ createPostgresqlPool 
    (configDatabaseUrl config) 
    (configMaxConnections config)
  case result of
    Right pool -> return $ Right pool
    Left (e :: SomeException) -> return $ Left $ DatabaseError $ 
      "Failed to create database connection pool: " <> show e

runMigrations :: ConnectionPool -> IO (Either AppError ())
runMigrations pool = do
  result <- try $ runSqlPool (runMigration migrateAll) pool
  case result of
    Right _ -> return $ Right ()
    Left (e :: SomeException) -> return $ Left $ DatabaseError $ 
      "Database migration failed: " <> show e

runDbAction :: ConnectionPool -> SqlPersistT IO a -> IO (Either AppError a)
runDbAction pool action = do
  result <- try $ runSqlPool action pool
  case result of
    Right value -> return $ Right value
    Left (e :: SomeException) -> return $ Left $ DatabaseError $ 
      "Database operation failed: " <> show e

insertTransaction :: Transaction -> SqlPersistT IO (Key Transaction)
insertTransaction = insert

getTransaction :: Text -> SqlPersistT IO (Maybe Transaction)
getTransaction txnId = get (TransactionKey txnId)

updateTransactionStatus :: Text -> TransactionStatus -> SqlPersistT IO ()
updateTransactionStatus txnId status = do
  now <- liftIO getCurrentTime
  update (TransactionKey txnId) 
    [ TransactionStatus =. transactionStatusToText status
    , TransactionUpdatedAt =. now
    ]

insertAuditLog :: AuditLog -> SqlPersistT IO (Key AuditLog)
insertAuditLog = insert

getAuditLogsForTransaction :: Text -> SqlPersistT IO [Entity AuditLog]
getAuditLogsForTransaction txnId = 
  selectList [AuditLogTransactionId ==. Just txnId] [Asc AuditLogCreatedAt]

getTransactionAnalytics :: SqlPersistT IO TransactionAnalytics
getTransactionAnalytics = do
  allTransactions <- selectList [] []
  let transactions = map entityVal allTransactions
      total = length transactions
      successCount = length $ filter (\t -> transactionStatus t == "success") transactions
      fraudCount = length $ filter transactionFraudFlag transactions
      successRate = if total > 0 then fromIntegral successCount / fromIntegral total * 100 else 0
      fraudRate = if total > 0 then fromIntegral fraudCount / fromIntegral total * 100 else 0
      avgAmount = if total > 0 then fromIntegral (sum $ map transactionAmount transactions) / fromIntegral total else 0
  
  return $ TransactionAnalytics
    { taTotal = total
    , taSuccessRate = successRate  
    , taFraudRate = fraudRate
    , taAverageAmount = avgAmount
    }

data TransactionAnalytics = TransactionAnalytics
  { taTotal :: Int
  , taSuccessRate :: Double
  , taFraudRate :: Double
  , taAverageAmount :: Double
  } deriving (Show, Eq, Generic)

instance FromJSON TransactionStatus

instance ToJSON TransactionAnalytics where
  toJSON (TransactionAnalytics total successRate fraudRate avgAmount) = object
    [ "total" .= total
    , "successRate" .= successRate
    , "fraudRate" .= fraudRate
    , "averageAmount" .= avgAmount
    ]