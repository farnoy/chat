{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE CPP                  #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}

module Chat.Application(
  app
  ) where

import Chat.Combinators
import Chat.Persistence
import Control.Concurrent.STM.TQueue
import Control.Concurrent.Async
import Control.Exception
import Control.Monad
import qualified Control.Monad.Catch as C
import Control.Monad.STM
import Control.Monad.Trans
import Control.Monad.Trans.Either
import Control.Monad.Trans.Reader
import Data.Aeson
import Data.ByteString.Lazy (toStrict)
import Data.ByteString.Conversion
import Data.Int
import Data.List (nub, find)
import Data.Maybe
import Data.Monoid ((<>))
import Data.Time.Calendar
import Data.Time.Clock
import Data.Typeable
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding
import Database.Persist
import Database.Persist.Sql
import GHC.Generics
import Network.HTTP.Types.Header (hContentType)
import Network.Wai
import Servant

data Env = Env { db :: ConnectionPool, queue :: TQueue Text }
type AppM = ReaderT Env (EitherT ServantErr IO)
type App a = AppM a

withDb :: SqlPersistT AppM a -> App a
withDb f = do
  env <- ask
  runSqlPool f (db env)

announce :: Text -> App ()
announce msg = do
  env <- ask
  _ <- liftIO . async . atomically . writeTQueue (queue env) $ msg
  return ()

appError :: forall a b. ToJSON a => a -> App b
appError = lift . appError_

appError_ :: forall a b. ToJSON a => a -> EitherT ServantErr IO b
appError_ a = left err500 { errBody = encode a, errHeaders = [(hContentType, "application/json")] }

handlerToEither' :: Env -> AppM a -> EitherT ServantErr IO a
handlerToEither' e h = C.handle handler $ runReaderT h e
                       where handler :: SomeException -> EitherT ServantErr IO a
                             handler ex = do
                              liftIO (print $ "ERROR " <> show ex)
                              appError_ $ ApiStatusResult False (T.pack $ show ex)

handlerToEither :: Env -> (AppM :~> EitherT ServantErr IO)
handlerToEither e = Nat (handlerToEither' e)

newtype UserDay = UserDay Day deriving(Eq, Show)

data ChannelsApiResult = ChannelsApiResult (Entity Channel) deriving(Generic)

instance ToJSON ChannelsApiResult where
  toJSON (ChannelsApiResult (Entity k (Channel name))) = object ["id" .= k, "name" .= name]

data MessagesApiResult = MessagesApiResult (Key Message) User Message deriving(Generic)

instance ToJSON MessagesApiResult where
  toJSON (MessagesApiResult (MessageKey k) a (Message (ChannelKey ck) (UserKey ak) b t) )
         = object ["id" .= k, "body" .= b, "channel_id" .= ck,
                   "author_id" .= ak, "timestamp" .= t, "author" .= toJSON a]

instance ToJSON User where
  toJSON (User l _) = object ["login" .= l]

instance FromJSON Channel where
  parseJSON (Object v) = Channel <$> v .: "name"
  parseJSON _ = mzero

data ApiStatusResult = ApiStatusResult Bool Text

instance ToJSON ApiStatusResult where
  toJSON (ApiStatusResult ok err) = object ["ok" .= ok, "err" .= err]

data MessageInput = MessageInput Text

instance FromJSON MessageInput where
  parseJSON (Object v) = MessageInput <$> v .: "body"
  parseJSON _ = mzero

data UserInput = UserInput Text Text

instance FromJSON UserInput where
  parseJSON (Object v) = UserInput <$> v .: "login" <*> v .: "password"
  parseJSON _ = mzero

type Api = "channels" :> Get '[JSON] [ChannelsApiResult]
      :<|> "channels" :> WithCookie "session_id" Text :> ReqBody '[JSON] Channel :> Post '[JSON] ApiStatusResult
      :<|> "channels" :> WithCookie "session_id" Text :> Capture "channel" Text :> Delete '[JSON] ApiStatusResult
      :<|> "channels" :> Capture "channel" Text :> QueryParam "limit" Int :> "messages" :> Get '[JSON] [MessagesApiResult]
      :<|> "channels" :> Capture "channel" Text :> "messages" :> WithCookie "session_id" Int64 :> ReqBody '[JSON] MessageInput :> Post '[JSON] ApiStatusResult
      :<|> "signup" :> ReqBody '[JSON] UserInput :> Post '[JSON] (Headers '[Header "Set-Cookie" Text] ApiStatusResult)
      :<|> "signin" :> ReqBody '[JSON] UserInput :> Post '[JSON] (Headers '[Header "Set-Cookie" Text] ApiStatusResult)

channelsIndex :: App [ChannelsApiResult]
channelsIndex = do
                 channels <- withDb $
                   selectList [] []
                 return $ fmap ChannelsApiResult channels

channelsCreate :: Text -> Channel -> App ApiStatusResult
channelsCreate _ input = do
                          r <- C.try $ withDb $ insert_ input

                          case r of
                            Left (_ :: SomeException) -> appError (ApiStatusResult False "Channel with this name already exists")
                            Right _ -> return (ApiStatusResult True T.empty)

channelsDelete :: Text -> Text -> App ApiStatusResult
channelsDelete _ cn = do
                        withDb $ deleteWhere [ChannelChannelName ==. cn]

                        return $ ApiStatusResult True ""

messagesIndex :: Text -> Maybe Int -> App [MessagesApiResult]
messagesIndex cid limit = do
                        messages <- withDb $ do
                          (Just e) <- selectFirst [ChannelChannelName ==. cid] []
                          selectList [MessageChannelKey ==. entityKey e] [Asc MessageTimestamp, LimitTo $ clamp limit]

                        let authorIds = nub . fmap (messageAuthorKey . entityVal) $ messages

                        authors <- withDb $ selectList [UserId <-. authorIds] []

                        let findAuthor p = entityVal . fromJust . find ((==p) . entityKey) $ authors

                        let messagesWithAuthors = fmap (\(Entity k m) -> MessagesApiResult k (findAuthor (messageAuthorKey m)) m) messages

                        return messagesWithAuthors
              where clamp = maybe 100 (min 100 . max 1)

messagesCreate :: Text -> Int64 -> MessageInput -> App ApiStatusResult
messagesCreate cid sid (MessageInput b) = do
                        time <- liftIO getCurrentTime
                        res <- withDb $ do
                          Just (Entity channel _) <- selectFirst [ChannelChannelName ==. cid] []
                          key <- insert (Message channel (toSqlKey sid) b time)
                          Just (Entity _ message) <- selectFirst [MessageId ==. key] []
                          Just author <- get $ messageAuthorKey message
                          return $ MessagesApiResult key author message

                        announce $ decodeUtf8 $ toStrict $ encode res

                        return (ApiStatusResult True "")

signup :: UserInput -> App (Headers '[Header "Set-Cookie" Text] ApiStatusResult)
signup (UserInput l p) = do
                          res <- C.try $ withDb $ insert (User l (toByteString' p))

                          case res of
                            Right k -> return $ addHeader (sessionCookie (fromSqlKey k)) (ApiStatusResult True "")
                            Left (_ :: SomeException) -> appError $ ApiStatusResult False "Login taken"

signin :: UserInput -> App (Headers '[Header "Set-Cookie" Text] ApiStatusResult)
signin (UserInput l p) = do
                          user <- withDb $
                            selectFirst [UserLogin ==. l, UserEncryptedPassword ==. toByteString' p] []

                          case user of
                            Just (Entity k _) ->
                              return $ addHeader (sessionCookie (fromSqlKey k)) $
                                  ApiStatusResult True ""
                            Nothing -> appError $ ApiStatusResult False "Invalid password"

server :: Env -> Server Api
server e = enter (handlerToEither e)
               $  channelsIndex
             :<|> channelsCreate
             :<|> channelsDelete
             :<|> messagesIndex
             :<|> messagesCreate
             :<|> signup
             :<|> signin

sessionCookie :: ToText k => k -> Text
sessionCookie k = "session_id=" <> toText k <> "; Path=/; Max-Age=21600; HttpOnly"

api :: Proxy Api
api = Proxy

app :: ConnectionPool -> TQueue Text -> Application
app pool q = serve api $ server $ Env pool q
