{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeApplications #-}
module Professor.DumbRoutes where

import Proton
import Data.Profunctor
import Data.Profunctor.Arrow
import Control.Arrow.Profunctor
import Network.Wai
import Network.HTTP.Types.Method
import Network.HTTP.Types.Status
import Control.Category ((<<<), (>>>), Category)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Aeson
import Data.Bifunctor
import Data.Profunctor.Absorbent
import Control.Monad.IO.Class
import Data.Profunctor.Strong
import qualified Control.Arrow as A

-- _Method :: (Category p, Choice p) => Method -> Optic p Request Response Request Response -> Optic p Request Response Request Response -- p a b -> p Request Response
-- _Method m h p =
--     lmap matcher ((h p) ||| p)
--   where
--     matcher r
--       | (requestMethod r == m) = Right r
--       | otherwise = Left r

-- _Method m = _ . lmap matcher . left'
--   where
--     matcher r
--       | requestMethod r == m = Left r
--       | otherwise = Right r

-- _Exact :: String -> Prism Request Response Request Response
-- _Exact s =

-- method :: (Category p, Choice p, ProfunctorZero p) => Method -> (p Request Response -> p Request Response) -> (p Request Response -> p Request Response)
-- method m = match ((== m) . requestMethod)

-- pathPrefix :: (Category p, Choice p, ProfunctorZero p) => String -> (p Request Response -> p Request Response) -> (p Request Response -> p Request Response)
-- pathPrefix path = match (BS.isPrefixOf (BS.pack path) . rawPathInfo)

-- match :: (Category p, Choice p, ProfunctorZero p) => (Request -> Bool) -> (p Request Response -> p Request Response) -> (p Request Response -> p Request Response)
-- match predicate subHandler app' = lmap matcher (subHandler app' ||| app')
--   where
--     matcher r
--       | predicate r = Left r
--       | otherwise = Right r

-- method' :: (Category p, Choice p, ProfunctorZero p) => Method -> (p Request Response -> p Request Response) -> (p Request Response -> p Request Response)
-- method' m = match ((== m) . requestMethod)

-- pathPrefix' :: (Category p, Choice p, ProfunctorZero p) => String -> (p Request Response -> p Request Response) -> (p Request Response -> p Request Response)
-- pathPrefix' path = match (BS.isPrefixOf (BS.pack path) . rawPathInfo)

-- match' :: (Category p, Choice p, ProfunctorZero p) => (Request -> Bool) -> p Request Response -> (p Request Response) -> (p Request Response)
-- match' predicate handler def = lmap matcher (handler ||| def)
--   where
--     matcher r
--       | predicate r = Left r
--       | otherwise = Right r

-- method'' :: (Category p, Choice p, ProfunctorZero p) => Method -> p Request Response -> (p Request Response) -> (p Request Response)
-- method'' m = match' ((== m) . requestMethod)

-- pathPrefix'' :: (Category p, Choice p, ProfunctorZero p) => String -> p Request Response -> (p Request Response) -> (p Request Response)
-- pathPrefix'' path = match' (BS.isPrefixOf (BS.pack path) . rawPathInfo)


match :: Choice p => (Request -> Bool) -> (p Request (Either Request Response)) -> (p Request (Either Request Response))
match predicate subHandler = dimap matcher resolve (right' subHandler)
  where
    resolve (Left r) = Left r
    resolve (Right (Right r)) = Right r
    resolve (Right (Left r)) = Left r
    matcher r
      | predicate r = Right r
      | otherwise = Left r

method :: (Category p, Choice p) => Method -> (p Request (Either Request Response)) -> (p Request (Either Request Response))
method m = match ((== m) . requestMethod)

pathPrefix :: (Category p, Choice p) => String -> (p Request (Either Request Response)) -> (p Request (Either Request Response))
pathPrefix path = match (BS.isPrefixOf (BS.pack path) . rawPathInfo)


(|+|) :: (Category p, Choice p) => p a (Either x b) -> p x (Either y b) -> p a (Either y b)
l |+| r = rmap unify (l >>> left' r)
  where
    unify (Left (Left req)) = Left req
    unify (Left (Right resp)) = Right resp
    unify (Right resp) = Right resp

withDefault :: (Category p, Choice p) => p Request Response -> p Request (Either Request Response) -> p Request Response
withDefault def handler =
    rmap (either id id) (left' def <<< handler)

route :: Profunctor p => p Request Response -> p Request (Either Request Response)
route = rmap Right

json :: forall p m a b. (FromJSON a, ToJSON b, Choice p, Strong p, Category p, MonadIO m, Absorbent m p) => p a (Either a b) -> p Request (Either Request Response)
json p = rmap (second toResp) $ strong fixed (right' p <<< parsed)
    where
      toResp b = responseLBS ok200 [] $ encode b
      fixed _ (Right (Right resp)) = Right resp
      fixed req _ = Left req
      parsed :: p Request (Either String a)
      parsed = (absorb $ arr @p (\r -> eitherDecode' @a <$> liftIO (strictRequestBody r)))


json' :: forall p m a b. (FromJSON a, ToJSON b, Choice p, Strong p, Category p, MonadIO m, Absorbent m p) => p a (Either a b) -> p Request (Either Request Response)
json' (WrappedProfunctor -> handler) = unwrapProfunctor $ proc req -> do
    bytes <- reqBody -< req
    case eitherDecode' bytes of
        Left _ ->
            A.returnA -< Left req
        Right a -> do
            res <- handler -< a
            case res of
                Left _ ->
                    A.returnA -< Left req
                Right b ->
                    A.returnA -< Right (responseLBS ok200 [] (encode b))
  where
    reqBody :: WrappedProfunctor p Request BL.ByteString
    reqBody = WrappedProfunctor (absorb . arr $ liftIO . strictRequestBody)
