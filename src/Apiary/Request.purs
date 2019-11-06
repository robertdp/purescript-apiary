module Apiary.Request where

import Prelude

import Apiary.Response (class DecodeResponse, decodeResponse)
import Apiary.Types (Error, Request)
import Control.Monad.Except (runExcept)
import Data.Either (Either)
import Data.Symbol (SProxy(..))
import Effect.Aff (Aff)
import Milkis (fetch, headers, statusCode, text) as Milkis
import Milkis.Impl.Window (windowFetch) as Milkis
import Record as Record
import Type.Proxy (Proxy(..))

class BuildRequest route params body rep | route -> params body rep where
  buildRequest :: route -> params -> body -> Request

makeRequest :: forall route params body rep response.
  BuildRequest route params body rep =>
  DecodeResponse rep response =>
  route ->
  (Request -> Request) ->
  params ->
  body ->
  Aff (Either Error response)
makeRequest route transform params body = decode <$> fetch request
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

  decode = runExcept <<< decodeResponse (Proxy :: _ rep)
