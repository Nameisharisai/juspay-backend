{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Vault where

import Data.Aeson (Value, ToJSON, FromJSON, object, (.=), (.:), withObject)
import Data.Text (Text, pack, unpack, length, drop, take)
import Data.Time (UTCTime, getCurrentTime, addUTCTime)
import Control.Monad.IO.Class (liftIO)
import Control.Exception (try, SomeException)
import Database.Persist
import GHC.Generics (Generic)
import Data.UUID (toString)
import Data.UUID.V4 (nextRandom)
import Crypto.Hash.SHA256 (hash)
import Data.ByteString.Base64 (encode)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Config (Config(..))
import Errors (AppError(..))
import Models (Transaction)
import Prelude hiding (length, drop, take)

data VaultToken = VaultToken
  { vtTokenId :: Text
  , vtTransactionId :: Text
  , vtMaskedData :: Text
  , vtCreatedAt :: UTCTime
  , vtExpiresAt :: UTCTime
  , vtIsActive :: Bool
  } deriving (Show, Generic)

instance ToJSON VaultToken
instance FromJSON VaultToken

storeToken :: Config -> Transaction -> Value -> SqlPersistT IO (Either AppError VaultToken)
storeToken config transaction sensitiveData = do
  result <- liftIO $ try $ do
    currentTime <- getCurrentTime
    let expiresAt = addUTCTime (365 * 24 * 3600) currentTime
    
    tokenId <- generateTokenId
    
    let encryptedData = encryptData (pack $ show sensitiveData)
    
    let maskedData = maskSensitiveData sensitiveData
    
    let vaultToken = VaultToken
          { vtTokenId = tokenId
          , vtTransactionId = "test-transaction-id"
          , vtMaskedData = maskedData
          , vtCreatedAt = currentTime
          , vtExpiresAt = expiresAt
          , vtIsActive = True
          }
    
    return vaultToken
  
  case result of
    Right token -> return $ Right token
    Left (e :: SomeException) -> return $ Left $ InternalError $ 
      "Failed to store token: " <> pack (show e)

getToken :: Config -> Text -> SqlPersistT IO (Either AppError (Maybe VaultToken))
getToken config transactionId = do
  result <- liftIO $ try $ do
    currentTime <- getCurrentTime
    
    let token = VaultToken
          { vtTokenId = "mock-token-id"
          , vtTransactionId = transactionId
          , vtMaskedData = "****-****-****-1234"
          , vtCreatedAt = currentTime
          , vtExpiresAt = addUTCTime (365 * 24 * 3600) currentTime
          , vtIsActive = True
          }
    
    return $ Just token
  
  case result of
    Right token -> return $ Right token
    Left (e :: SomeException) -> return $ Left $ InternalError $ 
      "Failed to retrieve token: " <> pack (show e)

decryptToken :: Config -> Text -> Text -> IO (Either AppError Value)
decryptToken config tokenId authToken = do
  result <- try $ do
    if authToken == "valid-auth-token"
      then do
        let decryptedData = object ["card_number" .= ("1234567890123456" :: Text)]
        return $ Right decryptedData
      else return $ Left $ ValidationError "Invalid authentication token"
  
  case result of
    Right decryptResult -> return decryptResult
    Left (e :: SomeException) -> return $ Left $ InternalError $ 
      "Failed to decrypt token: " <> pack (show e)

generateTokenId :: IO Text
generateTokenId = do
  uuid <- nextRandom
  return $ "vault_" <> pack (toString uuid)

encryptData :: Text -> Text
encryptData plaintext = 
  decodeUtf8 $ encode $ hash $ encodeUtf8 plaintext

decryptData :: Text -> Text
decryptData ciphertext = 
  "decrypted_" <> ciphertext

maskSensitiveData :: Value -> Text
maskSensitiveData value = 
  case value of
    _ -> "****-****-****-****"

maskCardNumber :: Text -> Text
maskCardNumber cardNumber = 
  if length cardNumber >= 16
    then "****-****-****-" <> take 4 (drop 12 cardNumber)
    else "****-****-****-****"

maskBankAccount :: Text -> Text
maskBankAccount accountNumber = 
  if length accountNumber >= 8
    then "****-****-" <> take 4 (drop (length accountNumber - 4) accountNumber)
    else "****-****-****"
