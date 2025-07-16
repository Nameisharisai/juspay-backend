{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Servant
import Network.Wai.Handler.Warp (run)
import Data.Aeson (Value, ToJSON, FromJSON, object, (.=))
import Data.Text (Text)
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Lazy as BSL

import Config (getConfig, Config(..))
import Errors (AppError(..), toServantError)

data PaymentRequest = PaymentRequest
  { amount :: Int
  , currency :: Text
  } deriving (Show)

instance FromJSON PaymentRequest where
  parseJSON = withObject "PaymentRequest" $ \o -> PaymentRequest
    <$> o .: "amount"
    <*> o .: "currency"

data PaymentResponse = PaymentResponse
  { paymentId :: Text
  , status :: Text
  } deriving (Show)

instance ToJSON PaymentResponse where
  toJSON (PaymentResponse pid status) = object
    [ "paymentId" .= pid
    , "status" .= status
    ]

type API = 
       "api" :> "v1" :> "create-payment" :> ReqBody '[JSON] PaymentRequest :> Post '[JSON] PaymentResponse
  :<|> "health" :> Get '[JSON] Value

server :: Config -> Server API
server config = createPaymentHandler :<|> healthHandler
  where
    createPaymentHandler req = do
      liftIO $ putStrLn $ "Processing payment for amount: " ++ show (amount req)
      return $ PaymentResponse "test-payment-id" "pending"
    
    healthHandler = return $ object ["status" .= ("healthy" :: Text)]

main :: IO ()
main = do
  putStrLn "Starting Juspay Backend (Minimal Version)..."
  
  config <- getConfig
  putStrLn $ "Starting server on port " ++ show (configPort config)
  
  let app = serve (Proxy :: Proxy API) (server config)
  run (configPort config) app