{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Main where

import Chat.Application
import Chat.Persistence
import Chat.WebSocket
import Control.Concurrent
import Control.Monad.IO.Class  (liftIO)
import Control.Monad.Logger    (runStderrLoggingT)
import Control.Concurrent.STM.TQueue
import Database.Persist.Postgresql
import Network.Wai.Handler.Warp

main :: IO ()
main = do
  websocketQueue <- newTQueueIO
  _ <- forkIO $ runWebSocket websocketQueue
  runStderrLoggingT $ withPostgresqlPool "postgres://chat@localhost/chat" 5 $ \p -> do
    runSqlPool (runMigration migrateAll) p
    liftIO $ run 8081 (app p websocketQueue)

