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
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TE
import qualified Database.PostgreSQL.Simple as PGS
import qualified Di
import DiPolysemy
import LNRPC.User (insertUser)
import Lens.Micro
import qualified Polysemy as P

-- Incomplete bot token. Do not hack pls
botToken :: Text
botToken = "MjUwMTM4OTE4MjgyOTg1NDcz.Gt973s.086SUxFoBEaDYNamAqdhhN6xry3zu1jgp5qv"

doRegisterUser :: PGS.Connection -> Text -> Text -> IO Int64
doRegisterUser = insertUser

run :: Pool PGS.Connection -> IO ()
run pool = Di.new $ \di ->
  void
    . P.runFinal
    . P.embedToFinal @IO
    . runDiToIO di
    . runCacheInMemory
    . runMetricsNoop
    . useFullContext
    . useConstantPrefix "Z-"
    . runBotIO (BotToken botToken) defaultIntents
    $ do
      info @Text "Setting up commands and handlers..."

      addCommands $ do
        helpCommand

        command @'[Text] "register" $ \ctx nodeURL -> do
          liftIO
            . withResource pool
            $ \conn -> doRegisterUser conn nodeURL (TL.toStrict . TE.decodeUtf8 . A.encode $ ctx ^. (#user . #id))

          void . invoke $ CreateReaction (ctx ^. #message) (ctx ^. #message) (UnicodeEmoji "💚")
