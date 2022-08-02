{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module LNRPC.Bot where

import Calamity
import Calamity.Cache.InMemory
import Calamity.Commands
import Calamity.Commands.Context (FullContext, useFullContext)
import Calamity.Metrics.Noop
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson as A
import Data.Default
import Data.Generics.Labels ()
import Data.Int (Int64)
import Data.Maybe
import Data.Pool (Pool, withResource)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Database.PostgreSQL.Simple as PGS
import qualified Di
import DiPolysemy
import LNRPC.Invoice (InvoiceResponse (..), createInvoice, runInvoiceRequest)
import LNRPC.User (Macaroon, MemoryCert, insertUser, runUserQuery, updateUserMac, updateUserTLS, userByUsername)
import Lens.Micro
import qualified Polysemy as P

-- Incomplete bot token. Do not hack pls
botToken :: Text
botToken = "MjUwMTM4OTE4MjgyOTg1NDcz.GvaedL.rgYSvlH_FgHDxbVlFcY4L6XZLAtjUderMCZg"

run :: Pool PGS.Connection -> IO ()
run pool = Di.new $ \di ->
  void
    . P.runFinal
    . P.embedToFinal @IO
    . runDiToIO di
    . runCacheInMemory
    . runMetricsNoop
    . useFullContext
    . useConstantPrefix "?"
    . runBotIO (BotToken botToken) defaultIntents
    $ do
      info @Text "Setting up commands and handlers..."

      addCommands $ do
        helpCommand

        command @'[Text] "register" $ \ctx nodeURL -> do
          liftIO
            . withResource pool
            $ \conn -> insertUser conn nodeURL (TL.toStrict . TLE.decodeUtf8 . A.encode $ ctx ^. (#user . #id))

          void . invoke $ CreateReaction (ctx ^. #message) (ctx ^. #message) (UnicodeEmoji "💚")

        command @'[Text] "addCert" $ \ctx cert -> do
          liftIO
            . withResource pool
            $ \conn -> updateUserTLS conn (TE.encodeUtf8 cert) (TL.toStrict . TLE.decodeUtf8 . A.encode $ ctx ^. (#user . #id))

          void . invoke $ CreateReaction (ctx ^. #message) (ctx ^. #message) (UnicodeEmoji "💘")

        command @'[Text] "addMac" $ \ctx mac -> do
          liftIO
            . withResource pool
            $ \conn -> updateUserMac conn (TE.encodeUtf8 mac) (TL.toStrict . TLE.decodeUtf8 . A.encode $ ctx ^. (#user . #id))

          void . invoke $ CreateReaction (ctx ^. #message) (ctx ^. #message) (UnicodeEmoji "💘")

        command @'[Text] "invoice" $ \ctx invoiceValue -> do
          userRes <- liftIO
            . withResource pool
            $ \conn -> runUserQuery conn (userByUsername (TL.toStrict . TLE.decodeUtf8 . A.encode $ ctx ^. (#user . #id)))

          commandResponse <- liftIO $ case userRes of
            [] -> return "You're not registered yet, please use the `register` command."
            (user : _) -> do
              let invoice = createInvoice Nothing invoiceValue Nothing
              invoice <- runInvoiceRequest user invoice

              case invoice of
                Nothing -> return "Could not create invoice"
                (Just invoice) -> return $ invoiceResponsePaymentRequest invoice

          void . tell @Text (ctx ^. #message) $ commandResponse
