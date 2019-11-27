module Apiary.Server.Response.Helper where

import Prelude
import Apiary.Media (class MediaCodec)
import Apiary.Route (class PrepareSpec, Route)
import Apiary.Server.Response (FullResponse, respondWithMedia)
import Apiary.Status (class ResponseStatus, toStatus)
import Data.Symbol (class IsSymbol, SProxy(..))
import Effect.Class (class MonadEffect)
import Prim.Row (class Cons, class Lacks)
import Prim.RowList (class RowToList, kind RowList, Cons, Nil)
import Record.Builder (Builder)
import Record.Builder as Builder
import Type.Data.RowList (RLProxy(..))
import Type.Proxy (Proxy(..), Proxy2(..))

class BuildResponder route m responder | route m -> responder where
  buildResponder :: route -> Proxy2 m -> responder

instance buildResponders ::
  ( PrepareSpec
      spec
      { params :: params
      , query :: query
      , body :: body
      , response :: Record responses
      }
  , RowToList responses responseList
  , BuildResponderRecord responseList m responders
  ) =>
  BuildResponder (Route method path spec) m { | responders } where
  buildResponder _ _ = Builder.build (buildResponderRecord (RLProxy :: _ responseList) (Proxy2 :: _ m)) {}

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
  , Cons status (response -> FullResponse m) responders' responders
  , BuildResponderRecord responseList m responders'
  ) =>
  BuildResponderRecord (Cons status responseRep responseList) m responders where
  buildResponderRecord _ m = Builder.insert status responder <<< buildResponderRecord (RLProxy :: _ responseList) m
    where
    status = SProxy :: _ status

    responder = respondWithMedia (toStatus status) (Proxy :: _ responseRep)
