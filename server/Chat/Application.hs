{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE TypeOperators        #-}
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
import Chat.Persistence.Generated
import Control.Concurrent.STM.TQueue
import Control.Exception
import Control.Monad
import Control.Monad.STM
import Control.Monad.Trans
import Control.Monad.Trans.Either
import Data.Aeson
import Data.ByteString.Lazy (toStrict)
import Data.ByteString.Conversion
import Data.List (nub, find)
import Data.Monoid ((<>))
import Data.Pool
import Data.Time.Calendar
import Data.Time.Clock
import Data.Typeable
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding
import Database.Groundhog
import Database.Groundhog.Core
import Database.Groundhog.Postgresql
import Database.PostgreSQL.Simple.Internal
import GHC.Generics
import Network.Wai
import Servant

newtype UserDay = UserDay Day deriving(Eq, Show)

data ChannelsApiResult = ChannelsApiResult (Key Channel BackendSpecific) Channel deriving(Generic)

instance ToJSON ChannelsApiResult where
  toJSON (ChannelsApiResult (ChannelKey k) (Channel name)) = object ["id" .= k, "name" .= name]

data MessagesApiResult = MessagesApiResult (Key Message BackendSpecific) User Message deriving(Generic)

instance ToJSON MessagesApiResult where
  toJSON (MessagesApiResult (MessageKey k) a (Message (ChannelKey ck) (UserKey ak) b t) )
         = object ["id" .= k, "body" .= b, "channel_id" .= ck,
                   "author_id" .= ak, "timestamp" .= t, "author" .= toJSON a]

instance ToJSON User where
  toJSON (User l _) = object ["login" .= l]

instance ToJSON Message where
  toJSON (Message (ChannelKey ck) (UserKey ak) b t) = object ["channel_id" .= ck, "author_id" .= ak, "timestamp" .= t, "body" .= b]

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

instance FromFormUrlEncoded UserInput where
  fromFormUrlEncoded form = case UserInput <$> lookup "login" form <*> lookup "password" form of
                              Just u -> Right u
                              _ -> Left ""

type Api =
      "channels" :> (
             Get '[JSON] [ChannelsApiResult]
        :<|> WithCookie "session_id" Text :> ReqBody '[JSON] Channel :> Post '[JSON] ApiStatusResult
        :<|> Capture "channel" Text :> "messages" :> Get '[JSON] [MessagesApiResult]
        :<|> Capture "channel" Text :> "messages" :> WithCookie "session_id" Integer :> ReqBody '[JSON] MessageInput :> Post '[JSON] ApiStatusResult
      )
      :<|> "signup" :> ReqBody '[JSON] UserInput :> Post '[JSON] (Headers '[Header "Set-Cookie" Text] ApiStatusResult)
      :<|> "signin" :> ReqBody '[JSON] UserInput :> Post '[JSON] (Headers '[Header "Set-Cookie" Text] ApiStatusResult)


server :: Pool Postgresql -> TQueue Text -> Server Api
server pool queue = (channelsList :<|> channelsCreate :<|> messagesIndex :<|> messagesCreate)
              :<|> signup :<|> signin
      where channelsList = do
                             channels <- withResource pool $ runDbConn $
                               project (AutoKeyField, ChannelConstructor) CondEmpty
                             return $ fmap (uncurry ChannelsApiResult) channels
            channelsCreate c input = do
                                    liftIO $ print c
                                    r <- liftIO
                                           ((try $
                                             do withResource pool $ runDbConn $ insert_ input
                                                return (ApiStatusResult True T.empty))
                                                :: IO (Either SqlError ApiStatusResult))

                                    case r of
                                      Left _ -> left err500
                                      Right r' -> return r'
            messagesIndex cid = do
                                    messages <- withResource pool $ runDbConn $ do
                                      (channel : _) <- project (AutoKeyField) (ChannelNameField ==. cid)
                                      project (AutoKeyField, MessageConstructor) (ChannelKeyField ==. channel)

                                    let authorIds = nub . fmap (\(_, m) -> authorKey m) $ messages
                                    let conditions = foldr (\a ac -> ac ||. (AutoKeyField ==. a)) CondEmpty authorIds

                                    authors <- withResource pool $ runDbConn $ do
                                      project (AutoKeyField, UserConstructor) conditions

                                    let findAuthor (UserKey p)  = let Just (_, f) = find (q . fst) authors in f
                                                        where q (UserKey k) = k == p

                                    let messagesWithAuthors = fmap (\(k, m) -> MessagesApiResult k (findAuthor (authorKey m)) m) messages

                                    return messagesWithAuthors
            messagesCreate cid sid (MessageInput b) = do
                                    time <- liftIO getCurrentTime
                                    res <- withResource pool $ runDbConn $ do
                                      (channel : _) <- project (AutoKeyField) (ChannelNameField ==. cid)
                                      key <- insert (Message channel (UserKey (PersistInt64 (fromIntegral sid))) b time)
                                      (MessageKey k, message) : _ <- project (AutoKeyField, MessageConstructor) (AutoKeyField ==. key)
                                      author : _ <- project UserConstructor (AutoKeyField ==. (authorKey message))
                                      return $ MessagesApiResult key author message

                                    liftIO $ atomically $ writeTQueue queue (decodeUtf8 $ toStrict $ encode res)
                                    return (ApiStatusResult True "")
            signup (UserInput l p) = do
                                      res <- liftIO ((try $withResource pool $ runDbConn $
                                        insert (User l (toByteString' p)))
                                        :: IO (Either SqlError (Key User BackendSpecific)))

                                      case res of
                                        Right (UserKey (PersistInt64 k)) -> return $ addHeader (sessionCookie k) (ApiStatusResult True "")
                                        _ -> return $ addHeader "" (ApiStatusResult False "Login taken")
            signin (UserInput l p) = do
                                      user <- withResource pool $ runDbConn $ do
                                        project AutoKeyField ((LoginField ==. l) &&. (EncryptedPasswordField ==. (toByteString' p)))

                                      case user of
                                        UserKey (PersistInt64 k) : _ ->
                                          return $ addHeader (sessionCookie k) $
                                              ApiStatusResult True ""
                                        _ -> return $ addHeader "" (ApiStatusResult False "Invalid password")

sessionCookie :: ToText k => k -> Text
sessionCookie k = "session_id=" <> toText k <> "; Path=/; Max-Age=21600; HttpOnly"

api :: Proxy Api
api = Proxy

app :: Pool Postgresql -> TQueue Text -> Application
app pool queue = serve api (server pool queue)
