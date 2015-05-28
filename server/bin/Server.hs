{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Chat.Application
import Chat.Persistence
import Chat.WebSocket
import Control.Concurrent
import Control.Monad.STM
import Control.Concurrent.STM.TQueue
import Data.Pool
import Database.Groundhog
import Database.Groundhog.Postgresql
import Network.Wai.Handler.Warp

main :: IO ()
main = do
  websocketQueue <- newTQueueIO
  forkIO $ runWebSocket websocketQueue
  pool <- createPostgresqlPool "postgres://chat@localhost/chat" 5
  withResource pool $ runDbConn $ runMigration $ do
    migrate (undefined :: Channel)
    migrate (undefined :: Message)
    migrate (undefined :: User)
  run 8081 (app pool websocketQueue)

