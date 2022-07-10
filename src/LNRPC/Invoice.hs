{-# LANGUAGE OverloadedStrings #-}

module LNRPC.Invoice where

import Data.Aeson
  ( FromJSON (..),
    ToJSON (..),
    decodeStrict,
    object,
    withObject,
    (.:),
    (.=),
  )
import Data.ByteString (ByteString)
import Data.ByteString.Base64 (decodeBase64)
import qualified Data.ByteString.Char8 as B8
import Data.Either (fromRight)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Hexdump (simpleHex)
import LNRPC.HTTP (Macaroon, MemoryCert, makeRequestStream)
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
      InvoiceUpdate memo preImg (toHex rHash) value isSettled settleDate paymentReq

toHex :: Text -> Text
toHex = T.pack . filter (/= ' ') . simpleHex . fromRight "" . decodeBase64 . encodeUtf8

-----------------------------------
createInvoice :: Maybe Text -> Text -> Maybe Text -> InvoiceRequest
createInvoice = InvoiceRequest

invoiceURL :: Port -> String
invoiceURL port = "https://localhost:" <> show port <> "/v1/invoices/subscribe"

subscribeToInvoice :: Macaroon -> MemoryCert -> Port -> IO ()
subscribeToInvoice mac cert port = do
  (req, manager) <- makeRequestStream "GET" "" mac cert (invoiceURL port)
  withResponse req manager $ \response -> do
    let loop = do
          chunk <- brRead $ responseBody response
          case decodeStrict chunk :: Maybe InvoiceUpdate of
            Just v -> do
              B8.hPutStrLn stdout (encodeUtf8 $ invoiceUpdateHash v)
              loop
            Nothing -> loop

    loop
