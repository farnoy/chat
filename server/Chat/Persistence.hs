{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}

module Chat.Persistence where

import Data.Text (Text)
import Data.Time.Clock
import Data.ByteString.Char8 (ByteString)
import Database.Groundhog
import Database.Groundhog.Postgresql()
import GHC.Generics

data Channel = Channel {
  channelName :: Text
} deriving(Eq, Show, Generic)

data User = User {
  login :: Text,
  encryptedPassword :: ByteString
}

data Message = Message {
  channelKey :: DefaultKey Channel,
  authorKey :: DefaultKey User,
  contents :: Text,
  timestamp :: UTCTime
}
