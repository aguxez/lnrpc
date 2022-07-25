{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module LNRPC.Bot where

import Calamity
import Calamity.Cache.InMemory
import Calamity.Commands
import Calamity.Commands.Context (useFullContext)
import Calamity.Metrics.Noop
import Control.Monad
import Data.Default
import Data.Generics.Labels ()
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Di
import DiPolysemy
import Lens.Micro
import qualified Polysemy as P

-- Incomplete bot token.
botToken :: Text
botToken = "MjUwMTM4OTE4MjgyOTg1NDcz.Gt973s.086SUxFoBEaDYNamAqdhhN6xry3zu1jgp5qv"

run :: IO ()
run = Di.new $ \di ->
  void
    . P.runFinal
    . P.embedToFinal @IO
    . runDiToIO di
    . runCacheInMemory
    . runMetricsNoop
    . useFullContext
    . useConstantPrefix "!"
    . runBotIO (BotToken botToken) defaultIntents
    $ do
      info @Text "Setting up commands and handlers..."

      react @'MessageCreateEvt $ \(msg, _, _) -> do
        when ("Haskell" `T.isInfixOf` (msg ^. #content)) $
          void . invoke $ CreateReaction msg msg (UnicodeEmoji "🆗")

-- addCommands $ do
--   helpCommand
--   command @'[Int, Maybe GuildChannel] "slowmode" $ \ctx seconds mchan -> do
--     let cid = maybe (ctx ^. #channel . to getID) getID mchan :: Snowflake Channel
--     void . invoke $ ModifyChannel cid $ def & #rateLimitPerUser ?~ seconds
--     void . invoke $ CreateReaction (ctx ^. #channel) (ctx ^. #message) (UnicodeEmoji "✅")
