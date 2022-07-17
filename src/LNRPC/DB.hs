{-# LANGUAGE OverloadedStrings #-}

module LNRPC.DB where

import qualified Data.ByteString.Char8 as B8
import Data.Pool (Pool, PoolConfig (..), newPool)
import Database.PostgreSQL.Simple (Connection, close, connectPostgreSQL)

type ConnectionString = B8.ByteString

connStr :: ConnectionString
connStr = "dbname=lnrpc_dev user=postgres password=postgres"

makeDBConnection :: IO Connection
makeDBConnection = connectPostgreSQL connStr

closeDBConnection :: Connection -> IO ()
closeDBConnection = close

createDBPool :: IO (Pool Connection)
createDBPool = do
  let poolConfig =
        PoolConfig
          { createResource = makeDBConnection,
            freeResource = closeDBConnection,
            poolCacheTTL = 1,
            poolMaxResources = 50
          }

  newPool poolConfig
