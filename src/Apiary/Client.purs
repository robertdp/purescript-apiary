module Apiary.Client
  ( makeRequest
  , fetch
  , module Apiary.Media
  , module Apiary.Types
  ) where

import Prelude
import Apiary.Client.Request (class BuildRequest, buildRequest)
import Apiary.Client.Response (class DecodeResponse, decodeResponse)
import Apiary.Media (class DecodeMedia, class EncodeMedia, class MediaType, JSON, None, decodeMedia, encodeMedia, mediaType, none)
import Apiary.Types (Error(..), Request, Response, emptyRequest)
import Control.Comonad (extract)
import Control.Monad.Error.Class (try)
import Control.Monad.Except (ExceptT(..), mapExceptT, runExceptT, withExceptT)
import Data.Either (Either)
import Effect.Aff (Aff)
import Milkis (fetch, headers, statusCode, text) as Milkis
import Milkis.Impl.Window (windowFetch)
import Type.Proxy (Proxy(..))

makeRequest ::
  forall route params query body rep response.
  BuildRequest route params query body rep =>
  DecodeResponse rep response =>
  route ->
  (Request -> Request) ->
  params ->
  query ->
  body ->
  Aff (Either Error response)
makeRequest route transform params query body = runExceptT $ decode =<< fetch request
  where
  request :: Request
  request = transform $ buildRequest route params query body

  decode :: Response -> ExceptT Error Aff response
  decode text = mapExceptT (pure <<< extract) $ decodeResponse (Proxy :: _ rep) text

lift :: Aff ~> ExceptT Error Aff
lift = withExceptT RuntimeError <<< ExceptT <<< try

fetch :: Request -> ExceptT Error Aff Response
fetch request@{ method, url, headers } = do
  response <-
    lift case request.body of
      "" -> Milkis.fetch windowFetch url { method, headers }
      body -> Milkis.fetch windowFetch url { method, headers, body }
  text <- lift $ Milkis.text response
  pure
    { status: Milkis.statusCode response
    , headers: Milkis.headers response
    , body: text
    }
