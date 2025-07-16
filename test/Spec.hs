{-# LANGUAGE OverloadedStrings #-}

module Main where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Data.Text (Text)
import Data.Aeson (Value, object, (.=))
import System.Environment (setEnv, unsetEnv)

import Config
import Validation
import Fraud
import Router
import Gateways
import Models
import Errors

main :: IO ()
main = hspec $ do
  describe "Input Validation" $ do
    validationTests
  
  describe "Fraud Detection" $ do
    fraudDetectionTests
  
  describe "Payment Router" $ do
    routerTests
  
  describe "Payment Gateways" $ do
    gatewayTests
  
  describe "Database Models" $ do
    modelTests
  
  describe "Configuration" $ do
    configTests
  
  describe "Error Handling" $ do
    errorTests

validationTests :: Spec
validationTests = do
  describe "Amount validation" $ do
    it "accepts positive amounts" $ do
      mkValidatedAmount 1000 `shouldBe` Right (ValidatedAmount 1000)
    
    it "rejects zero amount" $ do
      mkValidatedAmount 0 `shouldSatisfy` isLeft
    
    it "rejects negative amounts" $ do
      mkValidatedAmount (-100) `shouldSatisfy` isLeft
    
    it "rejects excessive amounts" $ do
      mkValidatedAmount 20000000 `shouldSatisfy` isLeft
  
  describe "Currency validation" $ do
    it "accepts valid ISO 4217 currencies" $ do
      mkValidatedCurrency "USD" `shouldBe` Right (ValidatedCurrency "USD")
      mkValidatedCurrency "INR" `shouldBe` Right (ValidatedCurrency "INR")
    
    it "rejects invalid currency codes" $ do
      mkValidatedCurrency "US" `shouldSatisfy` isLeft
      mkValidatedCurrency "USDD" `shouldSatisfy` isLeft
      mkValidatedCurrency "" `shouldSatisfy` isLeft
  
  describe "IP validation" $ do
    it "accepts non-empty IP addresses" $ do
      mkValidatedIP "192.168.1.1" `shouldBe` Right (ValidatedIP "192.168.1.1")
    
    it "rejects empty IP addresses" $ do
      mkValidatedIP "" `shouldSatisfy` isLeft
  
  describe "Country validation" $ do
    it "accepts valid country codes" $ do
      mkValidatedCountry "US" `shouldBe` Right (ValidatedCountry "US")
    
    it "rejects empty country codes" $ do
      mkValidatedCountry "" `shouldSatisfy` isLeft

fraudDetectionTests :: Spec
fraudDetectionTests = do
  describe "Amount-based fraud detection" $ do
    it "detects high-risk transactions" $ do
      let highAmount = checkAmount 600000
      checkScore highAmount `shouldSatisfy` (> 20.0)
    
    it "allows normal transactions" $ do
      let normalAmount = checkAmount 50000
      checkScore normalAmount `shouldSatisfy` (< 10.0)
  
  describe "Geographic fraud detection" $ do
    it "flags high-risk countries" $ do
      let geoCheck = checkGeographic "XX"
      checkScore geoCheck `shouldSatisfy` (> 15.0)

routerTests :: Spec
routerTests = do
  describe "Processor selection" $ do
    it "finds available processors for valid amounts" $ do
      let processors = filter (\p -> pcEnabled p && 
                                   1000 >= pcMinAmount p && 
                                   1000 <= pcMaxAmount p) 
                             availableProcessors
      length processors `shouldSatisfy` (> 0)
    
    it "excludes disabled processors" $ do
      let disabledProcessors = filter (not . pcEnabled) availableProcessors
      length disabledProcessors `shouldSatisfy` (> 0)

gatewayTests :: Spec
gatewayTests = do
  describe "Gateway configuration validation" $ do
    it "validates Instamojo config with credentials" $ do
      let validConfig = Config 8080 "db" "redis" "kafka" 
                              "api_key" "auth_token" "https://api.instamojo.com" 
                              Info 10
      validateGatewayConfig validConfig "instamojo" `shouldBe` Right ()
    
    it "rejects Instamojo config without credentials" $ do
      let invalidConfig = Config 8080 "db" "redis" "kafka" 
                                "" "" "https://api.instamojo.com" 
                                Info 10
      validateGatewayConfig invalidConfig "instamojo" `shouldSatisfy` isLeft
    
    it "rejects unknown processors" $ do
      let config = Config 8080 "db" "redis" "kafka" 
                         "api_key" "auth_token" "https://api.instamojo.com" 
                         Info 10
      validateGatewayConfig config "unknown" `shouldSatisfy` isLeft

modelTests :: Spec
modelTests = do
  describe "Transaction status" $ do
    it "converts status to text correctly" $ do
      transactionStatusToText Success `shouldBe` "success"
      transactionStatusToText Failed `shouldBe` "failed"
      transactionStatusToText Pending `shouldBe` "pending"

configTests :: Spec
configTests = do
  describe "Environment variable loading" $ do
    it "loads default configuration when no env vars set" $ do
      mapM_ unsetEnv ["PORT", "POSTGRES_URL", "REDIS_URL"]
      config <- loadConfig
      configPort config `shouldBe` 8080
    
    it "loads configuration from environment variables" $ do
      setEnv "PORT" "9000"
      setEnv "POSTGRES_URL" "postgresql://test"
      setEnv "REDIS_URL" "redis://test"
      config <- loadConfig
      configPort config `shouldBe` 9000

errorTests :: Spec
errorTests = do
  describe "Error conversion" $ do
    it "converts validation errors to 400" $ do
      let serverError = toServantError (ValidationError "test")
      show serverError `shouldContain` "400"
    
    it "converts fraud errors to 403" $ do
      let serverError = toServantError (FraudDetected "test")
      show serverError `shouldContain` "403"
    
    it "converts other errors to 500" $ do
      let serverError = toServantError (InternalError "test")
      show serverError `shouldContain` "500"

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft (Right _) = False
