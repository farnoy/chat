{-# LANGUAGE OverloadedStrings #-}

module Chat.WebSocket where

import Control.Exception
import Control.Monad
import Control.Monad.STM
import Control.Concurrent
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM.TQueue
import Data.Text (Text)
import Network.WebSockets
import System.Random

server :: TVar [(Integer, TQueue Text)] -> PendingConnection -> IO ()
server state pending = do
  conn <- acceptRequest pending
  queue <- newTQueueIO
  rand <- randomIO

  -- add the client to the state
  atomically (modifyTVar state ((rand, queue):))

  flip finally
    -- remove from the list when done
    (atomically $ modifyTVar state (filter $ (/= rand) . fst))

    $ forever $ do
      msg <- atomically (readTQueue queue)
      sendTextData conn msg


broadcast :: TQueue Text -> TVar [(Integer, TQueue Text)] -> IO ()
broadcast q state = forever $ do
  msg <- atomically (readTQueue q)
  clients <- atomically (readTVar state)
  print (length clients)
  sequence $ (atomically . flip writeTQueue msg . snd) `fmap` clients

runWebSocket :: TQueue Text -> IO ()
runWebSocket q = do
  state <- newTVarIO []
  _ <- forkIO (broadcast q state)
  runServer "127.0.0.1" 8082 (server state)
