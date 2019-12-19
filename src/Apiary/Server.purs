module Apiary.Server where

import Prelude
import Apiary.Server.Request (class DecodeRequest, Request, decodeRequest)
import Apiary.Server.Request as Request
import Apiary.Server.Response (FullResponse)
import Apiary.Server.Response as Response
import Apiary.Server.Response.Helper (class BuildResponder, buildResponder)
import Apiary.Server.Router (class AttachToRouter, Router)
import Apiary.Server.Router as Router
import Apiary.Server.Url (PathParams)
import Apiary.Status as Status
import Apiary.Types (JSON)
import Control.Monad.Except (runExcept)
import Data.Array.NonEmpty as Array
import Data.Either (Either(..))
import Effect.Aff (Aff, launchAff_)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect)
import Foreign (MultipleErrors, renderForeignError)
import Node.HTTP as HTTP
import Type.Proxy (Proxy(..), Proxy2(..))

newtype Handler m route
  = Handler
  { route :: route
  , handler :: HTTP.Request -> HTTP.Response -> PathParams -> m Unit
  }

makeHandler ::
  forall route params body responder m.
  MonadAff m =>
  DecodeRequest route params body =>
  BuildResponder route m responder =>
  route ->
  (Request params body -> responder -> FullResponse m) ->
  Handler m route
makeHandler route handler = Handler { route, handler: routerHandler }
  where
  routerHandler httpRequest httpResponse pathParams = do
    requestBody <- liftAff $ Request.readBodyAsString httpRequest
    let
      queryParams = Request.requestQuery httpRequest

      responder = buildResponder route (Proxy2 :: _ m)

      updateHeaders = _ { headers = HTTP.requestHeaders httpRequest }
    decodeRequest route pathParams queryParams requestBody
      # runExcept
      # case _ of
          Right request -> Response.runResponse (handler (updateHeaders request) responder) httpResponse
          Left errs -> Response.runResponse (sendMultipleErrors errs) httpResponse

sendMultipleErrors :: forall m. MonadEffect m => MultipleErrors -> FullResponse m
sendMultipleErrors errs =
  Response.respondWithMedia Status.badRequest (Proxy :: _ (JSON _))
    { errors: Array.fromFoldable $ map renderForeignError $ errs }

attachToRouter ::
  forall m route.
  AttachToRouter route =>
  (m Unit -> Aff Unit) ->
  Handler m route ->
  Router Unit
attachToRouter runHandler (Handler { route, handler }) =
  Router.attachToRouter route \request response params ->
    launchAff_ $ runHandler $ handler request response params
