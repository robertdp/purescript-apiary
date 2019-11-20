module Apiary.Server.Response where

import Prelude
import Apiary.Media (class MediaCodec, encodeMedia, mediaType)
import Apiary.Route (class PrepareSpec, Route)
import Apiary.Server.Handler (Handler(..))
import Apiary.Status (class ResponseStatus, Status(..), toStatus)
import Control.Monad.Indexed.Qualified as Ix
import Data.Foldable (class Foldable, traverse_)
import Data.MediaType (MediaType)
import Data.Newtype (unwrap)
import Data.Symbol (class IsSymbol, SProxy(..))
import Data.Tuple (Tuple, uncurry)
import Effect.Class (class MonadEffect, liftEffect)
import Node.Encoding as Encoding
import Node.HTTP as HTTP
import Node.Stream as Stream
import Prim.Row (class Cons, class Lacks)
import Prim.RowList (class RowToList, kind RowList, Cons, Nil)
import Record.Builder (Builder)
import Record.Builder as Builder
import Type.Data.RowList (RLProxy(..))
import Type.Proxy (Proxy(..), Proxy2(..))

data StatusLineOpen

data HeadersOpen

data BodyOpen

data ResponseEnded

type Header
  = Tuple String String

type FullHandler m
  = Handler m StatusLineOpen ResponseEnded Unit

writeStatus :: forall m. MonadEffect m => Status -> Handler m StatusLineOpen HeadersOpen Unit
writeStatus (Status { code, reason }) =
  Handler \res ->
    liftEffect do
      HTTP.setStatusCode res code
      HTTP.setStatusMessage res reason

writeHeader :: forall m. MonadEffect m => String -> String -> Handler m HeadersOpen HeadersOpen Unit
writeHeader name value =
  Handler \res ->
    liftEffect do
      HTTP.setHeader res name value

closeHeaders :: forall m. Monad m => Handler m HeadersOpen BodyOpen Unit
closeHeaders = Handler \_ -> pure unit

headers :: forall f m. Foldable f => MonadEffect m => f Header -> Handler m HeadersOpen BodyOpen Unit
headers hs = Ix.do
  traverse_ (uncurry writeHeader) hs
  closeHeaders

contentType :: forall m. MonadEffect m => MediaType -> Handler m HeadersOpen HeadersOpen Unit
contentType mediaType = writeHeader "Content-Type" (unwrap mediaType)

withResponseStream ::
  forall m a.
  MonadEffect m =>
  (Stream.Writable () -> m a) ->
  Handler m BodyOpen ResponseEnded a
withResponseStream f =
  Handler \res -> do
    let
      s = HTTP.responseAsStream res
    a <- f s
    liftEffect do Stream.end s mempty
    pure a

send :: forall m. MonadEffect m => String -> Handler m BodyOpen ResponseEnded Unit
send str =
  withResponseStream \stream ->
    void
      $ liftEffect do
          Stream.writeString stream Encoding.UTF8 str mempty

respondWithMedia ::
  forall m rep a.
  MediaCodec rep a =>
  MonadEffect m =>
  Status ->
  Proxy rep ->
  a ->
  FullHandler m
respondWithMedia status rep response = Ix.do
  writeStatus status
  traverse_ contentType (mediaType rep)
  closeHeaders
  send (encodeMedia rep response)

class BuildResponder route responder | route -> responder where
  buildResponder :: route -> responder

instance buildResponders ::
  ( PrepareSpec
      spec
      { params :: params
      , query :: query
      , body :: body
      , response :: Record responses
      }
  , RowToList responses responseList
  , MonadEffect m
  , BuildResponderRecord responseList m responders
  ) =>
  BuildResponder (Route method path spec) { | responders } where
  buildResponder _ = Builder.build (buildResponderRecord (RLProxy :: _ responseList) (Proxy2 :: _ m)) {}

class BuildResponderRecord (responses :: RowList) (m :: Type -> Type) (responders :: #Type) | responses m -> responders where
  buildResponderRecord :: RLProxy responses -> Proxy2 m -> Builder {} { | responders }

instance buildResponderRecordNil :: BuildResponderRecord Nil m () where
  buildResponderRecord _ _ = identity

instance buildResponderRecordCons ::
  ( IsSymbol status
  , ResponseStatus status
  , MediaCodec responseRep response
  , MonadEffect m
  , Lacks status responders'
  , Cons status (response -> FullHandler m) responders' responders
  , BuildResponderRecord responseList m responders'
  ) =>
  BuildResponderRecord (Cons status responseRep responseList) m responders where
  buildResponderRecord _ m = Builder.insert status responder <<< buildResponderRecord (RLProxy :: _ responseList) m
    where
    status = SProxy :: _ status

    responder = respondWithMedia (toStatus status) (Proxy :: _ responseRep)
