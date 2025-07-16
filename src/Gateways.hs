{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Gateways where

import Data.Aeson (Value, ToJSON, FromJSON, object, (.=), (.:), withObject, parseJSON, Result(..))
import Data.Text (Text, pack)
import Data.Text.Encoding (encodeUtf8)
import Network.HTTP.Simple (httpJSON, parseRequest, getResponseBody, setRequestTimeout, HttpException, setRequestBodyJSON, setRequestHeader, Request, responseTimeoutMicro)
import Network.HTTP.Types.Status (statusCode)
import Control.Exception (try, bracket)
import Control.Monad.IO.Class (liftIO)
import Data.Time (addUTCTime, getCurrentTime)
import GHC.Generics (Generic)
import Errors (AppError(..))
import Config (Config(..))
import Validation (ValidatedPaymentRequest(..), getAmount, getCurrency)

data PaymentGatewayResponse = PaymentGatewayResponse
  { pgrPaymentId :: Text
  , pgrRedirectUrl :: Text
  , pgrStatus :: PaymentStatus
  , pgrGateway :: Text
  , pgrRawResponse :: Value
  } deriving (Show, Generic)

data PaymentStatus = Pending | Success | Failed | Cancelled deriving (Show, Eq, Generic)

instance ToJSON PaymentGatewayResponse
instance ToJSON PaymentStatus
instance FromJSON PaymentStatus

processPayment :: Config -> ValidatedPaymentRequest -> Text -> IO (Either AppError PaymentGatewayResponse)
processPayment config validatedReq processor = do
  case processor of
    "instamojo" -> processInstamojoPayment config validatedReq
    _ -> return $ Left $ PaymentGatewayError $ "Unsupported processor: " <> processor

processInstamojoPayment :: Config -> ValidatedPaymentRequest -> IO (Either AppError PaymentGatewayResponse)
processInstamojoPayment config validatedReq = do
  result <- try $ do
    let requestBody = object
          [ "purpose" .= ("Payment" :: Text)
          , "amount" .= getAmount (vprAmount validatedReq)
          , "currency" .= getCurrency (vprCurrency validatedReq)
          , "buyer_name" .= ("Customer" :: Text)
          , "email" .= ("customer@example.com" :: Text)
          , "phone" .= ("9999999999" :: Text)
          , "redirect_url" .= ("https://example.com/success" :: Text)
          , "send_email" .= False
          , "send_sms" .= False
          , "allow_repeated_payments" .= False
          ]
    
    let url = "POST https://api.instamojo.com/v2/payment_requests/"
    request <- parseRequest url
    let authenticatedRequest = setRequestHeaders 
          [ ("X-Api-Key", configInstamojoApiKey config)
          , ("X-Auth-Token", configInstamojoAuthToken config)
          , ("Content-Type", "application/json")
          ] 
          $ setRequestTimeout (30 * 1000000)
          $ setRequestBodyJSON requestBody request
    
    response <- httpJSON authenticatedRequest
    let responseData = getResponseBody response
    
    case parseInstamojoResponse responseData of
      Right gatewayResponse -> return $ Right gatewayResponse
      Left parseError -> return $ Left $ PaymentGatewayError $ 
        "Failed to parse Instamojo response: " <> parseError
  
  case result of
    Right paymentResult -> return paymentResult
    Left (e :: HttpException) -> return $ Left $ PaymentGatewayError $ 
      "Instamojo API error: " <> pack (show e)

parseInstamojoResponse :: Value -> Either Text PaymentGatewayResponse
parseInstamojoResponse responseValue = 
  case parseJSON responseValue of
    Success response -> Right response
    Error err -> Left $ pack err

instance FromJSON PaymentGatewayResponse where
  parseJSON = withObject "PaymentGatewayResponse" $ \o -> do
    success <- o .: "success"
    if success
      then do
        paymentRequest <- o .: "payment_request"
        paymentId <- paymentRequest .: "id"
        redirectUrl <- paymentRequest .: "longurl"
        return $ PaymentGatewayResponse
          { pgrPaymentId = paymentId
          , pgrRedirectUrl = redirectUrl
          , pgrStatus = Pending
          , pgrGateway = "instamojo"
          , pgrRawResponse = responseValue
          }
      else do
        message <- o .: "message"
        fail $ "Instamojo error: " ++ show message

setRequestHeaders :: [(Text, Text)] -> Request -> Request
setRequestHeaders headers req = foldr addHeader req headers
  where addHeader (name, value) = setRequestHeader (encodeUtf8 name) [encodeUtf8 value]

validateGatewayConfig :: Config -> Text -> Either AppError ()
validateGatewayConfig config processor = 
  case processor of
    "instamojo" -> 
      if configInstamojoApiKey config == "" || configInstamojoAuthToken config == ""
        then Left $ ValidationError "Instamojo API credentials not configured"
        else Right ()
    _ -> Left $ ValidationError $ "Unknown processor: " <> processor
