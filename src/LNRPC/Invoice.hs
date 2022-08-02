{-# LANGUAGE OverloadedStrings #-}

module LNRPC.Invoice where

import Control.Concurrent (ThreadId, forkIO)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson
  ( FromJSON (..),
    ToJSON (..),
    decode,
    decodeStrict,
    encode,
    object,
    withObject,
    (.:),
    (.=),
  )
import Data.ByteString (ByteString)
import Data.ByteString.Base64 (decodeBase64)
import qualified Data.ByteString.Char8 as B8
import Data.Either (fromRight)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import qualified Database.PostgreSQL.Simple as PGS
import Hexdump (simpleHex)
import LNRPC.HTTP (makeRequest, makeRequestStream)
import LNRPC.User
  ( Macaroon,
    MemoryCert,
    User,
    User' (..),
    runUserQuery,
    userByUsername,
  )
import Network.HTTP.Client (brRead, withResponse)
import Network.HTTP.Conduit (responseBody)
import System.IO (stdout)

type Port = Int

data InvoiceRequest = InvoiceRequest
  { invoiceRequestMemo :: Maybe Text,
    invoiceRequestValue :: Text,
    invoiceRequestDescriptionHash :: Maybe Text
  }
  deriving (Show)

instance ToJSON InvoiceRequest where
  toJSON (InvoiceRequest memo value descriptionHash) =
    object
      ["memo" .= memo, "value" .= value, "description_hash" .= descriptionHash]

-----------------------

data InvoiceResponse = InvoiceResponse
  { invoiceResponseHash :: Text,
    invoiceResponsePaymentRequest :: Text,
    invoiceResponseAddIndex :: Text,
    invoiceResponsePaymentAddress :: Text
  }
  deriving (Show)

instance FromJSON InvoiceResponse where
  parseJSON = withObject "InvoiceResponse" $ \x ->
    InvoiceResponse
      <$> x
      .: "r_hash"
      <*> x
      .: "payment_request"
      <*> x
      .: "add_index"
      <*> x
      .: "payment_addr"

-----------------------
data InvoiceUpdate = InvoiceUpdate
  { invoiceUpdateMemo :: Text,
    invoiceUpdatePreImage :: Text,
    invoiceUpdateHash :: Text,
    invoiceUpdateValue :: Text,
    isInvoiceSettled :: Bool,
    invoiceUpdateSettleDate :: Text,
    invoiceUpdatePaymentRequest :: Text
  }
  deriving (Show)

instance FromJSON InvoiceUpdate where
  parseJSON = withObject "InvoiceUpdate" $ \x -> do
    result <- x .: "result"
    memo <- result .: "memo"
    preImg <- result .: "r_preimage"
    rHash <- result .: "r_hash"
    value <- result .: "value"
    isSettled <- result .: "settled"
    settleDate <- result .: "settle_date"
    paymentReq <- result .: "payment_request"

    return $
      InvoiceUpdate memo preImg (toHex' rHash) value isSettled settleDate paymentReq

toHex' :: Text -> Text
toHex' = T.pack . filter (/= ' ') . simpleHex . fromRight "" . decodeBase64 . encodeUtf8

toHex :: ByteString -> ByteString
toHex = B8.pack . filter (/= ' ') . simpleHex

-----------------------------------
invoiceCreateURL :: Port -> String
invoiceCreateURL port = "https://localhost:" <> show port <> "/v1/invoices"

createInvoice :: Maybe Text -> Text -> Maybe Text -> InvoiceRequest
createInvoice = InvoiceRequest

handleCertFromDB :: Maybe Text -> B8.ByteString
handleCertFromDB Nothing = ""
handleCertFromDB (Just cert) = either encodeUtf8 id (decodeBase64 $ encodeUtf8 cert)

runInvoiceRequest :: User -> InvoiceRequest -> IO (Maybe InvoiceResponse)
runInvoiceRequest user invoice = do
  let url = T.unpack (userNodeURL user <> "/v1/invoices")
      mac = toHex . fromRight "" . decodeBase64 . encodeUtf8 . fromMaybe "" $ userMac user
      cert = handleCertFromDB $ userCert user
  res <- makeRequest "POST" (encode invoice) mac cert url
  return $ decode res

subscribeToInvoice :: PGS.Connection -> Text -> IO (Maybe ThreadId)
subscribeToInvoice conn username = do
  userRes <- liftIO $ runUserQuery conn (userByUsername username)
  case userRes of
    [] -> return Nothing
    (user : _) -> do
      thread <- forkIO $ subscribeToInvoice' user
      return (Just thread)

subscribeToInvoice' :: User -> IO ()
subscribeToInvoice' user = do
  let url = T.unpack $ userNodeURL user <> "/v1/invoices/subscribe"
      mac = toHex . fromRight "" . decodeBase64 . encodeUtf8 . fromMaybe "" $ userMac user
      cert = handleCertFromDB $ userCert user
  print mac
  (req, manager) <- makeRequestStream "GET" "" (mac :: Macaroon) (cert :: MemoryCert) url
  withResponse req manager $ \response -> do
    let loop = do
          chunk <- brRead $ responseBody response
          case decodeStrict chunk :: Maybe InvoiceUpdate of
            Just v -> do
              B8.hPutStrLn stdout (encodeUtf8 $ invoiceUpdateHash v)
              loop
            Nothing -> loop

    loop
