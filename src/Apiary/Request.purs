module Apiary.Request where

import Prelude
import Apiary.Response (class DecodeResponse, decodeResponse)
import Apiary.Types (Request)
import Control.Comonad (extract)
import Control.Monad.Except (runExcept, throwError)
import Data.Either (Either(..))
import Data.Symbol (SProxy(..))
import Effect.Aff (Aff, error)
import Effect.Class.Console as Console
import Foreign (renderForeignError)
import Milkis (fetch, headers, statusCode, text) as Milkis
import Milkis.Impl.Window (windowFetch) as Milkis
import Record as Record
import Type.Proxy (Proxy(..))

class Requestable route params body response | route -> params body response where
  makeRequest :: route -> (Request -> Request) -> params -> body -> Aff response

class BuildRequest route params body rep | route -> params body rep where
  buildRequest :: route -> params -> body -> Request

instance requestable ::
  ( BuildRequest route params body rep
  , DecodeResponse rep response
  ) =>
  Requestable route params body response where
  makeRequest route transform params body = decode =<< fetch request
    where
    request = transform $ buildRequest route params body

    fetch req = do
      response <- Milkis.fetch Milkis.windowFetch req.url $ Record.delete (SProxy :: _ "url") req
      text <- Milkis.text response
      pure
        { status: Milkis.statusCode response
        , headers: Milkis.headers response
        , body: text
        }

    decode =
      decodeResponse (Proxy :: _ rep)
        >>> runExcept
        >>> case _ of
            Left errors -> do
              Console.logShow $ renderForeignError <$> errors
              throwError $ error $ renderForeignError $ extract errors
            Right response -> pure response
