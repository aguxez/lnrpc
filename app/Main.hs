{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Aeson
  ( decode,
    encode,
  )
import qualified Data.ByteString as B
import Data.Text (Text)
import LNRPC.HTTP
  ( Macaroon,
    MemoryCert,
    makeRequest,
  )
import LNRPC.Invoice

aliceCert :: IO MemoryCert
aliceCert =
  B.readFile "/Users/migueldiaz/.polar/networks/1/volumes/lnd/alice/tls.cert"

alicePubKey :: Text
alicePubKey = "1JXZBFuc8trStJanVptXWyGQYDaGfZgdYu"

aliceMac :: IO Macaroon
aliceMac =
  B.readFile
    "/Users/migueldiaz/.polar/networks/1/volumes/lnd/alice/data/chain/bitcoin/regtest/admin.macaroon"

main :: IO ()
main = return ()
