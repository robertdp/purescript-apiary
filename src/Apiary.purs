module Apiary where

import Prelude
import Apiary.Request (class BuildRequest, buildRequest)
import Apiary.Response (class DecodeResponse, decodeResponse)
import Apiary.Types (Error(..), Request, Response)
import Control.Comonad (extract)
import Control.Monad.Error.Class (try)
import Control.Monad.Except (ExceptT(..), mapExceptT, runExceptT, withExceptT)
import Data.Either (Either)
import Data.String as String
import Effect.Aff (Aff)
import Milkis (fetch, headers, statusCode, text) as Milkis
import Milkis.Impl.Window (windowFetch) as Milkis
import Simple.JSON (undefined)
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

makeRequest ::
  forall route params body rep response.
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
  request =
    buildRequest route params body
      # transform
      # removeEmptyBody

  decode :: Response -> ExceptT Error Aff response
  decode text = mapExceptT (pure <<< extract) $ decodeResponse (Proxy :: _ rep) text

lift :: Aff ~> ExceptT Error Aff
lift = withExceptT RuntimeError <<< ExceptT <<< try

fetch :: Request -> ExceptT Error Aff Response
fetch { method, url, headers, body } = do
  response <- lift $ Milkis.fetch Milkis.windowFetch url { method, headers, body }
  text <- lift $ Milkis.text response
  pure
    { status: Milkis.statusCode response
    , headers: Milkis.headers response
    , body: text
    }

removeEmptyBody :: Request -> Request
removeEmptyBody request@{ body } =
  if String.null body then
    request { body = unsafeCoerce undefined }
  else
    request
