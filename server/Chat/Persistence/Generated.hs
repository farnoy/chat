{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric #-}

module Chat.Persistence.Generated where

import Database.Groundhog.TH
import Chat.Persistence

mkPersist defaultCodegenConfig [groundhog|
- entity: Channel
  dbName: channels
  constructors:
    - name: Channel
      fields:
        - name: channelName
          dbName: name
      uniques:
        - name: NameConstraint
          fields: [channelName]
- entity: Message
  dbName: messages
  constructors:
    - name: Message
      fields:
        - name: channelKey
          dbName: channel_id
        - name: authorKey
          dbName: author_id
- entity: User
  dbName: users
  constructors:
    - name: User
      fields:
        - name: encryptedPassword
          dbName: encrypted_password
      uniques:
        - name: LoginConstraint
          fields: [login]
|]

