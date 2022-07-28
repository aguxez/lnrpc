{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module LNRPC.User
  ( insertUser,
    runUserQuery,
    allUsersQuery,
    userByUsername,
    Macaroon,
    MemoryCert,
    User' (..),
    User,
  )
where

import Data.ByteString (ByteString)
import Data.ByteString.Base64 (encodeBase64)
import Data.Int (Int64)
import Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import Data.Text (Text)
import qualified Database.PostgreSQL.Simple as PGS
import Opaleye
  ( Field,
    FieldNullable,
    Insert (..),
    Select,
    SqlText,
    Table,
    Update,
    null,
    optionalTableField,
    requiredTableField,
    runSelect,
    selectTable,
    table,
    where_,
    (.===),
  )
import Opaleye.Manipulation (rCount, runInsert, runUpdate)
import Opaleye.MaybeFields (MaybeFields (..))
import Opaleye.SqlTypes (sqlStrictText)

data User' a b c d = User'
  { userHandler :: a,
    userCert :: b,
    userMac :: c,
    userNodeURL :: d
  }
  deriving (Show)

type User = User' Text (Maybe Text) (Maybe Text) Text

type UserField = User' (Field SqlText) (FieldNullable SqlText) (FieldNullable SqlText) (Field SqlText)

type Macaroon = ByteString

type MemoryCert = ByteString

$(makeAdaptorAndInstance "pUser" ''User')

userTable :: Table UserField UserField
userTable =
  table
    "users"
    ( pUser
        User'
          { userHandler = requiredTableField "username",
            userCert = requiredTableField "tls_cert",
            userMac = requiredTableField "macaroon",
            userNodeURL = requiredTableField "node_url"
          }
    )

runUserQuery :: PGS.Connection -> Select UserField -> IO [User]
runUserQuery = runSelect

allUsersQuery :: Select UserField
allUsersQuery = selectTable userTable

{-
The cert and macaroon are encoded to Base64 for convenience. This probably can be done in a different way.
The macaroon needs to be hex encoded when calling the node but we cannot save it as is on the DB because of some casting problems.

TODO: Encrypt cert and mac and only give access when a specific encoded key is given. Save key with expiry on Redis.
TODO: Encapsulate the params in a single type so this function can be more manageable.
-}
insertUser :: PGS.Connection -> Text -> Text -> IO Int64
insertUser conn nodeURL username = runInsert conn insert
  where
    insert :: Insert Int64
    insert =
      Insert
        { iTable = userTable,
          iReturning = rCount,
          iRows = [User' (sqlStrictText username) Opaleye.null Opaleye.null (sqlStrictText nodeURL)],
          iOnConflict = Nothing
        }

    toBase64 :: ByteString -> Field SqlText
    toBase64 = sqlStrictText . encodeBase64

-- updateUserTLS :: PGS.Connection -> Text -> IO Int64
-- updateUserTLS = runUpdate conn update
--   where
--     update :: Update Int64
--     update = Update
--       { uTable = userTable,
--         uUpdateWith = updateEasy (\() -> )
--       }

userByUsername :: Text -> Select UserField
userByUsername argUsername = do
  row@(User' username _ _ _) <- allUsersQuery
  where_ (username .=== sqlStrictText argUsername)
  pure row
