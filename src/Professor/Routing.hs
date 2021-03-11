{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}
module Professor.Routing where

import Proton
import Data.Profunctor
import Data.Profunctor.Arrow
import Network.Wai
import Network.HTTP.Types.Method
import Network.HTTP.Types.Status
import Control.Category ((>>>), Category)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Aeson
import Data.Profunctor.Absorbent
import Control.Monad.IO.Class
import Data.Void
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Encoding as T
import Data.Functor

-- Match on a path-prefix and strip it from all nested matchers
stripPrefix :: (Category p, Choice p, ProfunctorZero p) => BS.ByteString -> p Request Response -> p Request Response
stripPrefix prefix = simpleMatch embed matcher
  where
    matcher r = BS.stripPrefix prefix (rawPathInfo r) <&> \newPath -> r{rawPathInfo=newPath}
    embed = id

-- Lift a prism into a route matcher
matchPrism :: (Choice p, ProfunctorZero p, Category p) => Prism s t a b -> p a b -> p s t
matchPrism prsm p = withPrism prsm $ \embed matcher ->
    dimap matcher (either absurd embed) (zeroProfunctor +++ p)

-- Construct a prism-like given a projection and a matcher
simpleMatch :: (Category p, Choice p, ProfunctorZero p) => (b -> t) -> (s -> Maybe a) -> p a b -> p s t
simpleMatch embed matcher p = dimap (maybe (Left ()) Right . matcher) (either absurd embed) (zeroProfunctor +++ p)

-- A simple boolean predicate matcher
match :: (Choice p, ProfunctorZero p, Category p) => (Request -> Bool) -> p Request Response -> p Request Response
match predicate handler =
    dimap matcher (either absurd id) (zeroProfunctor +++ handler)
  where
    matcher r
      | predicate r = Right r
      | otherwise = Left r

-- Match the request method
method :: (Choice p, ProfunctorZero p, Category p) => Method -> p Request Response -> p Request Response
method m = match ((== m) . requestMethod)

-- Match on a path prefix without changing the request
pathPrefix :: (Category p, Choice p, ProfunctorZero p) => String -> (p Request Response) -> (p Request Response)
pathPrefix path = match (BS.isPrefixOf (BS.pack path) . rawPathInfo)

-- Lift a handler over JSON objects into one over Request/Response
json :: forall p m a b. (FromJSON a, ToJSON b, MonadIO m , Choice p, Category p, Absorbent m p, ProfunctorZero p)
     => p a b
     -> p Request Response
json p = asBytes (dimap eitherDecode (encode . either absurd id) (zeroProfunctor +++ p))

-- Lift a handler over Text into one over Request/Response
asText :: (Category p, MonadIO m, Absorbent m p) => p T.Text T.Text -> p Request Response
asText p = asBytes (dimap T.decodeUtf8 T.encodeUtf8 p)

-- Lift a handler over ByteStrings into one over Request/Response
asBytes :: (Category p, MonadIO m, Absorbent m p) => p BL.ByteString BL.ByteString -> p Request Response
asBytes p = absorb (arr (liftIO . strictRequestBody)) >>> p >>> arr (responseLBS ok200 [])

