{-# LANGUAGE OverloadedStrings #-}

module LNRPC.HTTP
  ( makeRequest,
    makeRequestStream,
    Macaroon,
    MemoryCert,
  )
where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy.Char8 as BL8
import Data.X509.CertificateStore (makeCertificateStore)
import Data.X509.Memory (readSignedObjectFromMemory)
import Hexdump (simpleHex)
import LNRPC.User (Macaroon, MemoryCert)
import Network.Connection (TLSSettings (..))
import Network.HTTP.Client
  ( Request (..),
    RequestBody (..),
    newManager,
    parseRequest,
  )
import Network.HTTP.Conduit
  ( Manager,
    ManagerSettings,
    httpLbs,
    mkManagerSettings,
  )
import Network.HTTP.Simple
  ( Header,
    getResponseBody,
  )
import Network.TLS
  ( ClientParams (..),
    HostName,
    Shared (..),
    Supported (..),
    defaultParamsClient,
  )
import Network.TLS.Extra.Cipher (ciphersuite_default)

macaroonHeader :: Macaroon -> Header
macaroonHeader mac = ("Grpc-Metadata-macaroon", mac)

mkManager :: HostName -> MemoryCert -> ManagerSettings
mkManager hostName cert = mkManagerSettings settings Nothing
  where
    defs = defaultParamsClient hostName ""

    settings =
      TLSSettings $
        defs
          { clientShared =
              (clientShared defs)
                { sharedCAStore =
                    makeCertificateStore $
                      readSignedObjectFromMemory cert
                },
            clientSupported =
              (clientSupported defs)
                { supportedCiphers = ciphersuite_default
                }
          }

makeRequest ::
  B.ByteString ->
  BL8.ByteString ->
  Macaroon ->
  MemoryCert ->
  String ->
  IO BL8.ByteString
makeRequest reqMethod payload macaroon cert requestURL = do
  manager <- newManager (mkManager "localhost" cert)
  initReq <- parseRequest requestURL
  let req =
        initReq
          { method = reqMethod,
            requestBody = RequestBodyLBS payload,
            requestHeaders = [macaroonHeader macaroon]
          }
  res <- httpLbs req manager
  return $ getResponseBody res

makeRequestStream ::
  B.ByteString ->
  BL8.ByteString ->
  Macaroon ->
  MemoryCert ->
  String ->
  IO (Request, Manager)
makeRequestStream reqMethod queryStr macaroon cert requestURL = do
  manager <- newManager (mkManager "localhost" cert)
  initReq <- parseRequest requestURL
  let baseReq =
        initReq
          { method = reqMethod,
            queryString = BL8.toStrict queryStr,
            requestHeaders = [macaroonHeader macaroon]
          }
  return (baseReq, manager)
