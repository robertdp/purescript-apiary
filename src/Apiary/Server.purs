module Apiary.Server where

import Prelude
import Apiary.Server.Handler (runHandler)
import Apiary.Server.Request (class DecodeRequest, Request, decodeRequest, readBodyAsString, requestQuery)
import Apiary.Server.Response (class BuildResponder, FullHandler, buildResponder, respondWithMedia)
import Apiary.Server.Router (class AttachToRouter, Router, attachToRouter)
import Apiary.Status as Status
import Apiary.Types (JSON)
import Control.Monad.Except (lift, runExcept)
import Control.Monad.Reader (ReaderT, ask)
import Data.Array as Array
import Data.Either (Either(..))
import Effect.Aff (Aff, launchAff_)
import Effect.Class (class MonadEffect)
import Foreign (MultipleErrors, renderForeignError)
import Type.Proxy (Proxy(..), Proxy2(..))

makeHandler ::
  forall route params body responder m.
  AttachToRouter route =>
  DecodeRequest route params body =>
  BuildResponder route m responder =>
  route ->
  (Request params body -> responder -> FullHandler m) ->
  ReaderT (m Unit -> Aff Unit) Router Unit
makeHandler route handler = do
  launch <- ask
  lift do
    attachToRouter route \httpRequest httpResponse pathParams ->
      launchAff_ do
        requestBody <- readBodyAsString httpRequest
        let
          queryParams = requestQuery httpRequest

          responder = buildResponder route (Proxy2 :: _ m)
        decodeRequest route pathParams queryParams requestBody
          # runExcept
          # case _ of
              Right request -> launch $ runHandler (handler request responder) httpResponse
              Left errs -> runHandler (sendMultipleErrors errs) httpResponse

sendMultipleErrors :: forall m. MonadEffect m => MultipleErrors -> FullHandler m
sendMultipleErrors errs =
  respondWithMedia Status.badRequest (Proxy :: _ (JSON _))
    { errors: Array.fromFoldable $ map renderForeignError $ errs }
