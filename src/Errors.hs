{-# LANGUAGE OverloadedStrings #-}

module Errors where

import Data.Text (Text)
import Data.Aeson (ToJSON(..), object, (.=))
import Servant (ServerError, err400, err403, err500, errBody)
import Data.Aeson (encode)

data AppError
  = ValidationError Text
  | FraudDetected Text
  | PaymentGatewayError Text
  | DatabaseError Text
  | InternalError Text
  deriving (Show, Eq)

instance ToJSON AppError where
  toJSON (ValidationError msg) = object ["error" .= ("validation_error" :: Text), "message" .= msg]
  toJSON (FraudDetected msg) = object ["error" .= ("fraud_detected" :: Text), "message" .= msg]
  toJSON (PaymentGatewayError msg) = object ["error" .= ("payment_gateway_error" :: Text), "message" .= msg]
  toJSON (DatabaseError msg) = object ["error" .= ("database_error" :: Text), "message" .= msg]
  toJSON (InternalError msg) = object ["error" .= ("internal_error" :: Text), "message" .= msg]

toServantError :: AppError -> ServerError
toServantError (ValidationError msg) = err400 { errBody = encode $ toJSON (ValidationError msg) }
toServantError (FraudDetected msg) = err403 { errBody = encode $ toJSON (FraudDetected msg) }
toServantError err = err500 { errBody = encode $ toJSON err }
