{-# LANGUAGE OverloadedStrings #-}
module Main where

import Professor.Routing
import Professor.Wai
import Network.Wai
import Network.HTTP.Types.Status
import Network.HTTP.Types.Method
import Data.Profunctor
import Data.Profunctor.Arrow
import Data.Profunctor.Absorbent
import qualified Control.Category as C
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Lazy as BL
import Control.Monad.IO.Class

notFoundHandler :: (C.Category p, Profunctor p) => p b Response
notFoundHandler = arr (const (responseLBS notFound404 [] "No match!"))

routes :: (Choice p, C.Category p, ProfunctorPlus p, MonadIO m, Absorbent m p) => p Request Response
routes =
        -- Simple path prefix matching
        (pathPrefix "/hello/" . method methodGet) sayHello
        -- Matchers can also act as middleware, stripPrefix removes part of the path from the request.
    <+> (method methodPost . stripPrefix "/transform/")
                -- Middleware and matchers are hand-in-hand, asText uplifts a Text -> Text
                -- handler into a proper request handler
          (     (pathPrefix "reverse" . asText) (arr TL.reverse)
            <+> (pathPrefix "truncate" . asText) (arr $ TL.take 10)
            -- Fall back to a simple echo server
            <+> asText C.id
          )

sayHello :: (Profunctor p, C.Category p) => p Request Response
sayHello = arr go
  where
    go = responseLBS ok200 [] . BL.fromStrict . T.encodeUtf8 . ("Hello " <>) . last . pathInfo

main :: IO ()
main = serve 8080 (routes <+> notFoundHandler)
