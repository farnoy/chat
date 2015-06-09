{-# LANGUAGE GADTs                        #-}
{-# LANGUAGE TypeFamilies                 #-}
{-# LANGUAGE TemplateHaskell              #-}
{-# LANGUAGE QuasiQuotes                  #-}
{-# LANGUAGE FlexibleInstances            #-}
{-# LANGUAGE MultiParamTypeClasses        #-}
{-# LANGUAGE GeneralizedNewtypeDeriving   #-}
{-# LANGUAGE StandaloneDeriving           #-}
{-# LANGUAGE DeriveGeneric                #-}

module Chat.Persistence where

import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Data.ByteString (ByteString)
import Database.Persist.TH

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Channel sql=channels
  channelName Text sql=name
  UniqueChannelName channelName
User sql=users
  login Text sql=login
  encryptedPassword ByteString sql=encrypted_password
  UniqueLogin login
Message sql=messages
  channelKey ChannelId sql=channel_id
  authorKey UserId sql=author_id
  contents Text
  timestamp UTCTime
|]
