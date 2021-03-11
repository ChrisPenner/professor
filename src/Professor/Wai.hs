module Professor.Wai where

import Network.Wai
import Network.Wai.Handler.Warp
import Data.Profunctor

type IOHandler = Star IO Request Response
-- type App p = Star IO Request Response

handlerToApplication :: IOHandler -> Application
handlerToApplication (Star f) req respond = f req >>= respond

serve :: Port -> IOHandler -> IO ()
serve p h = run p (handlerToApplication h)
