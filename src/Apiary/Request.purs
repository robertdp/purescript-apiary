module Apiary.Request where

import Prelude

import Apiary.Response (class DecodeResponse, decodeResponse)
import Apiary.Types (Error(..), Request, Response)
import Control.Comonad (extract)
import Control.Monad.Error.Class (try)
import Control.Monad.Except (ExceptT(..), mapExceptT, runExceptT, withExceptT)
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
makeRequest route transform params body = runExceptT $ decode =<< fetch request
  where
  request :: Request
  request = transform $ buildRequest route params body

  decode :: Response -> ExceptT Error Aff response
  decode text = mapExceptT (pure <<< extract) $ decodeResponse (Proxy :: _ rep) text

lift :: Aff ~> ExceptT Error Aff
lift = withExceptT RuntimeError <<< ExceptT <<< try

fetch :: Request -> ExceptT Error Aff Response
fetch req = do
  response <- lift $ Milkis.fetch Milkis.windowFetch req.url $ Record.delete (SProxy :: _ "url") req
  text <- lift $ Milkis.text response
  pure
    { status: Milkis.statusCode response
    , headers: Milkis.headers response
    , body: text
    }
