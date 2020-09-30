module Apiary.Client where

import Prelude
import Affjax (defaultRequest)
import Affjax as Affjax
import Affjax.RequestBody as RequestBody
import Affjax.ResponseFormat as ResponseFormat
import Apiary.Request (class BuildRequest, buildRequest)
import Apiary.Response (class DecodeResponse, decodeResponse)
import Apiary.Types (Error(..), Request, Response)
import Control.Comonad (extract)
import Control.Monad.Except (ExceptT(..), mapExceptT, runExceptT, withExcept, withExceptT)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
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
  decode text =
    mapExceptT (pure <<< extract)
      $ withExcept (_ $ request)
      $ decodeResponse (Proxy :: _ rep) text

fetch :: Request -> ExceptT Error Aff Response
fetch { method, url, headers, body } = do
  response <- withExceptT RuntimeError $ ExceptT runRequest
  pure
    { status: response.status
    , headers: response.headers
    , body: response.body
    }
  where
  runRequest =
    Affjax.request
      $ defaultRequest
          { method = Left method
          , url = url
          , headers = headers
          , responseFormat = ResponseFormat.string
          , content =
            case body of
              "" -> Nothing
              body' -> pure (RequestBody.String body')
          }
