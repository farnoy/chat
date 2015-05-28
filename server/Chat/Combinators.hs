{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module Chat.Combinators (
  Cookies(..),
  WithCookie
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding
import GHC.TypeLits (Symbol, KnownSymbol, symbolVal)
import Network.Wai
import Network.HTTP.Types (status401, status403)
import Servant
import Servant.Server.Internal (RouteMismatch(..), failWith)
import Text.Parsec hiding(try)
import Text.Parsec.Text

newtype Cookies = Cookies [(Text, Text)] deriving Show

cookieParser :: Parser Cookies
cookieParser = Cookies <$> many pair
               where pair :: Parser (Text, Text)
                     pair = (\a b -> (T.pack a, T.pack b)) <$> many1 (letter <|> char '_') <*> (char '=' *> many1 alphaNum)

instance FromText Cookies where
  fromText t = case parse cookieParser "" t of
                 Left _ -> Nothing
                 Right a -> Just a

data WithCookie (sym :: Symbol) a

instance (KnownSymbol sym, FromText a, HasServer sublayout)
         => HasServer (WithCookie sym a :> sublayout) where
  type ServerT (WithCookie sym a :> sublayout) m = a -> ServerT sublayout m

  route Proxy subserver request respond =
    case lookup "Cookie" (requestHeaders request) of
      Nothing -> respond $ failWith (HttpError status401 Nothing)
      Just x ->
        case parse cookieParser "" (decodeUtf8 x) of
          Left _ -> respond $ failWith (HttpError status403 Nothing)
          Right (Cookies cookies) ->
            case lookup (T.pack $ symbolVal (Proxy :: Proxy sym)) cookies of
              Nothing -> respond $ failWith (HttpError status403 Nothing)
              Just cookie ->
                case fromText cookie of
                  Nothing -> respond $ failWith (HttpError status403 Nothing)
                  Just value -> route (Proxy :: Proxy sublayout) (subserver value) request respond

