{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module LNRPC.User where

import Data.ByteString (ByteString)
import Data.ByteString.Base64 (encodeBase64)
import Data.Int (Int64)
import Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import Data.Text (Text)
import qualified Database.PostgreSQL.Simple as PGS
import Opaleye (Field, Insert (..), Select, SqlText, Table, runSelect, selectTable, table, tableField, where_, (.===))
import Opaleye.Manipulation (OnConflict (DoNothing), rCount, runInsert)
import Opaleye.SqlTypes (sqlStrictText)

data User' a b c = User'
  { userHandler :: a,
    userCert :: b,
    userMac :: c
  }
  deriving (Show)

type User = User' Text Text Text

type UserField = User' (Field SqlText) (Field SqlText) (Field SqlText)

type Macaroon = ByteString

type MemoryCert = ByteString

$(makeAdaptorAndInstance "pUser" ''User')

userTable :: Table UserField UserField
userTable = table "users" (pUser User' {userHandler = tableField "username", userCert = tableField "tls_cert", userMac = tableField "macaroon"})

allUsers :: Select UserField
allUsers = selectTable userTable

runUserQuery :: PGS.Connection -> Select UserField -> IO [User]
runUserQuery = runSelect

-- TODO: Encrypt cert and mac and only give access when a specific encoded key is given. Save key with expiry on Redis.
insertUser :: Text -> MemoryCert -> Macaroon -> Insert Int64
insertUser username cert mac =
  Insert {iTable = userTable, iRows = [User' (sqlStrictText username) (toBase64 cert) (toBase64 mac)], iReturning = rCount, iOnConflict = Just DoNothing}
  where
    toBase64 :: ByteString -> Field SqlText
    toBase64 = sqlStrictText . encodeBase64

runInsertUser :: PGS.Connection -> Insert Int64 -> IO Int64
runInsertUser = runInsert

userByUsername :: Text -> Select UserField
userByUsername argUsername = do
  row@(User' username _ _) <- allUsers
  where_ (username .=== sqlStrictText argUsername)
  pure row
