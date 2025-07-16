{-# LANGUAGE OverloadedStrings #-}

module Main where

import Criterion.Main
import Data.Aeson (Value, object, (.=))
import Data.Text (Text)

import Validation
import Fraud

main :: IO ()
main = defaultMain
  [ bgroup "Validation"
    [ bench "valid amount" $ whnf mkValidatedAmount 1000
    , bench "invalid amount" $ whnf mkValidatedAmount 0
    , bench "valid currency" $ whnf mkValidatedCurrency "USD"
    , bench "invalid currency" $ whnf mkValidatedCurrency "US"
    ]
  , bgroup "Fraud Detection"
    [ bench "amount check - low" $ whnf checkAmount 1000
    , bench "amount check - high" $ whnf checkAmount 600000
    , bench "geographic check - safe" $ whnf checkGeographic "US"
    , bench "geographic check - risky" $ whnf checkGeographic "XX"
    ]
  ]

sampleValidRequest :: Value
sampleValidRequest = object
  [ "amount" .= (1000 :: Int)
  , "currency" .= ("USD" :: Text)
  , "ip" .= ("192.168.1.1" :: Text)
  , "country" .= ("US" :: Text)
  ]

sampleInvalidRequest :: Value
sampleInvalidRequest = object
  [ "amount" .= (0 :: Int)
  , "currency" .= ("US" :: Text)
  , "ip" .= ("" :: Text)
  , "country" .= ("" :: Text)
  ]

sampleFraudInput :: (Int, Text, Text)
sampleFraudInput = (600000, "192.168.1.1", "XX")

simpleFraudCheck :: Int -> Bool
simpleFraudCheck amount = amount > 500000

complexFraudCheck :: (Int, Text, Text) -> Double
complexFraudCheck (amount, ip, country) = 
  let amountScore = if amount > 500000 then 25.0 else 0.0
      geoScore = if country == "XX" then 20.0 else 0.0
  in amountScore + geoScore
