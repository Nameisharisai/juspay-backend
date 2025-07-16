{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Validation where

import Data.Aeson (Value, fromJSON, Result(..), (.:))
import Data.Text (Text, length, strip, null)
import Data.Aeson.Types (Parser)
import Control.Monad (when, guard)
import GHC.Generics (Generic)
import Errors (AppError(..))
import Text.Read (readMaybe)
import Data.Char (isDigit, isAlpha)
import Prelude hiding (length, null)

data ValidatedPaymentRequest = ValidatedPaymentRequest
  { vprAmount :: ValidatedAmount
  , vprCurrency :: ValidatedCurrency
  , vprCardDetails :: Value
  , vprIpAddress :: ValidatedIP
  , vprCountry :: ValidatedCountry
  } deriving (Show, Generic)

newtype ValidatedAmount = ValidatedAmount Int deriving (Show, Eq)
newtype ValidatedCurrency = ValidatedCurrency Text deriving (Show, Eq)
newtype ValidatedIP = ValidatedIP Text deriving (Show, Eq)
newtype ValidatedCountry = ValidatedCountry Text deriving (Show, Eq)

mkValidatedAmount :: Int -> Either AppError ValidatedAmount
mkValidatedAmount amount
  | amount <= 0 = Left $ ValidationError "Amount must be positive"
  | amount > 10000000 = Left $ ValidationError "Amount exceeds maximum limit (10,000,000)"
  | otherwise = Right $ ValidatedAmount amount

mkValidatedCurrency :: Text -> Either AppError ValidatedCurrency
mkValidatedCurrency currency
  | null (strip currency) = Left $ ValidationError "Currency cannot be empty"
  | length currency /= 3 = Left $ ValidationError "Currency must be 3 characters (ISO 4217)"
  | not (all isAlpha (show currency)) = Left $ ValidationError "Currency must contain only letters"
  | otherwise = Right $ ValidatedCurrency currency

mkValidatedIP :: Text -> Either AppError ValidatedIP
mkValidatedIP ip
  | null (strip ip) = Left $ ValidationError "IP address cannot be empty"
  | length ip < 7 || length ip > 45 = Left $ ValidationError "Invalid IP address format"
  | otherwise = Right $ ValidatedIP ip

mkValidatedCountry :: Text -> Either AppError ValidatedCountry
mkValidatedCountry country
  | null (strip country) = Left $ ValidationError "Country cannot be empty"
  | length country /= 2 = Left $ ValidationError "Country must be 2 characters (ISO 3166-1)"
  | not (all isAlpha (show country)) = Left $ ValidationError "Country must contain only letters"
  | otherwise = Right $ ValidatedCountry country

validatePaymentRequest :: Value -> Either AppError ValidatedPaymentRequest
validatePaymentRequest val = do
  case fromJSON val of
    Error msg -> Left $ ValidationError $ "JSON parsing error: " <> (show msg)
    Success obj -> do
      amount <- case obj .: "amount" of
        Success a -> mkValidatedAmount a
        Error msg -> Left $ ValidationError $ "Invalid amount: " <> (show msg)
      
      currency <- case obj .: "currency" of
        Success c -> mkValidatedCurrency c
        Error msg -> Left $ ValidationError $ "Invalid currency: " <> (show msg)
      
      cardDetails <- case obj .: "cardDetails" of
        Success cd -> Right cd
        Error msg -> Left $ ValidationError $ "Invalid card details: " <> (show msg)
      
      ipAddress <- case obj .: "ipAddress" of
        Success ip -> mkValidatedIP ip
        Error msg -> Left $ ValidationError $ "Invalid IP address: " <> (show msg)
      
      country <- case obj .: "country" of
        Success c -> mkValidatedCountry c
        Error msg -> Left $ ValidationError $ "Invalid country: " <> (show msg)
      
      return $ ValidatedPaymentRequest amount currency cardDetails ipAddress country

getAmount :: ValidatedAmount -> Int
getAmount (ValidatedAmount a) = a

getCurrency :: ValidatedCurrency -> Text
getCurrency (ValidatedCurrency c) = c

getIP :: ValidatedIP -> Text
getIP (ValidatedIP ip) = ip

getCountry :: ValidatedCountry -> Text
getCountry (ValidatedCountry c) = c
