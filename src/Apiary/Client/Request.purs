module Apiary.Client.Request
  ( class BuildRequest
  , buildRequest
  ) where

import Prelude
import Affjax.RequestHeader (RequestHeader(..))
import Apiary.Client.Response (class DecodeResponse)
import Apiary.Client.Url (class BuildUrl, buildUrl)
import Apiary.Media (class EncodeMedia, class MediaType, encodeMedia, mediaType)
import Apiary.Route (class PrepareSpec, Route)
import Apiary.Types (None, Request)
import Data.Array as Array
import Data.Maybe (maybe)
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

class BuildRequest route params query body rep | route -> params query body rep where
  buildRequest :: route -> params -> query -> body -> Request

instance buildRequestRouteGET ::
  ( PrepareSpec
      spec
      { path :: params
      , query :: query
      , body :: None
      , response :: response
      }
  , BuildUrl params query
  , DecodeResponse response response'
  , IsSymbol path
  ) =>
  BuildRequest (Route "GET" path spec) params query None response where
  buildRequest _ = buildRequest_ "GET" (SProxy :: _ path) (Proxy :: _ None)
else instance buildRequestRoutePATCH ::
  ( PrepareSpec
      spec
      { path :: params
      , query :: query
      , body :: body
      , response :: response
      }
  , BuildUrl params query
  , MediaType body
  , EncodeMedia body body'
  , DecodeResponse response response'
  , IsSymbol path
  ) =>
  BuildRequest (Route "PATCH" path spec) params query body' response where
  buildRequest _ = buildRequest_ "PATCH" (SProxy :: _ path) (Proxy :: _ body)
else instance buildRequestRoutePOST ::
  ( PrepareSpec
      spec
      { path :: params
      , query :: query
      , body :: body
      , response :: response
      }
  , BuildUrl params query
  , MediaType body
  , EncodeMedia body body'
  , DecodeResponse response response'
  , IsSymbol path
  ) =>
  BuildRequest (Route "POST" path spec) params query body' response where
  buildRequest _ = buildRequest_ "POST" (SProxy :: _ path) (Proxy :: _ body)
else instance buildRequestRoutePUT ::
  ( PrepareSpec
      spec
      { path :: params
      , query :: query
      , body :: body
      , response :: response
      }
  , BuildUrl params query
  , MediaType body
  , EncodeMedia body body'
  , DecodeResponse response response'
  , IsSymbol path
  ) =>
  BuildRequest (Route "PUT" path spec) params query body' response where
  buildRequest _ = buildRequest_ "PUT" (SProxy :: _ path) (Proxy :: _ body)
else instance buildRequestRouteDELETE ::
  ( PrepareSpec
      spec
      { path :: params
      , query :: query
      , body :: body
      , response :: response
      }
  , BuildUrl params query
  , MediaType body
  , EncodeMedia body body'
  , DecodeResponse response response'
  , IsSymbol path
  ) =>
  BuildRequest (Route "DELETE" path spec) params query body' response where
  buildRequest _ = buildRequest_ "DELETE" (SProxy :: _ path) (Proxy :: _ body)

buildRequest_ ::
  forall path params query proxy bodyRep body.
  IsSymbol path =>
  BuildUrl params query =>
  MediaType bodyRep =>
  EncodeMedia bodyRep body =>
  String ->
  SProxy path ->
  proxy bodyRep ->
  params ->
  query ->
  body ->
  Request
buildRequest_ method path bodyRep params query body =
  { method: unsafeCoerce method
  , url: unsafeCoerce (buildUrl params query (reflectSymbol path))
  , headers: maybe [] (Array.singleton <<< ContentType) (mediaType bodyRep)
  , body: encodeMedia bodyRep body
  }
