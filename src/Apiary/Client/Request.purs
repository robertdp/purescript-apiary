module Apiary.Client.Request
  ( class BuildRequest
  , buildRequest
  ) where

import Prelude
import Apiary.Client.Response (class DecodeResponse)
import Apiary.Client.Url (class WriteParams, writeParams)
import Apiary.Media (class EncodeMedia, class MediaType, None, encodeMedia, mediaType)
import Apiary.Route (class PrepareSpec, Route)
import Apiary.Types (Request)
import Data.Maybe (maybe)
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Foreign.Object as Object
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

class BuildRequest route params body rep | route -> params body rep where
  buildRequest :: route -> params -> body -> Request

instance buildRequestRouteGET ::
  ( PrepareSpec
      spec
      { params :: params
      , query :: query
      , body :: None
      , response :: response
      }
  , WriteParams params query fullParams
  , DecodeResponse response response'
  , IsSymbol path
  ) =>
  BuildRequest (Route "GET" path spec) fullParams None response where
  buildRequest _ = buildRequest_ "GET" (SProxy :: _ path) (Proxy :: _ params) (Proxy :: _ query) (Proxy :: _ None)
else instance buildRequestRoutePATCH ::
  ( PrepareSpec
      spec
      { params :: params
      , query :: query
      , body :: body
      , response :: response
      }
  , WriteParams params query fullParams
  , MediaType body
  , EncodeMedia body body'
  , DecodeResponse response response'
  , IsSymbol path
  ) =>
  BuildRequest (Route "PATCH" path spec) fullParams body' response where
  buildRequest _ = buildRequest_ "PATCH" (SProxy :: _ path) (Proxy :: _ params) (Proxy :: _ query) (Proxy :: _ body)
else instance buildRequestRoutePOST ::
  ( PrepareSpec
      spec
      { params :: params
      , query :: query
      , body :: body
      , response :: response
      }
  , WriteParams params query fullParams
  , MediaType body
  , EncodeMedia body body'
  , DecodeResponse response response'
  , IsSymbol path
  ) =>
  BuildRequest (Route "POST" path spec) fullParams body' response where
  buildRequest _ = buildRequest_ "POST" (SProxy :: _ path) (Proxy :: _ params) (Proxy :: _ query) (Proxy :: _ body)
else instance buildRequestRoutePUT ::
  ( PrepareSpec
      spec
      { params :: params
      , query :: query
      , body :: body
      , response :: response
      }
  , WriteParams params query fullParams
  , MediaType body
  , EncodeMedia body body'
  , DecodeResponse response response'
  , IsSymbol path
  ) =>
  BuildRequest (Route "PUT" path spec) fullParams body' response where
  buildRequest _ = buildRequest_ "PUT" (SProxy :: _ path) (Proxy :: _ params) (Proxy :: _ query) (Proxy :: _ body)
else instance buildRequestRouteDELETE ::
  ( PrepareSpec
      spec
      { params :: params
      , query :: query
      , body :: body
      , response :: response
      }
  , WriteParams params query fullParams
  , MediaType body
  , EncodeMedia body body'
  , DecodeResponse response response'
  , IsSymbol path
  ) =>
  BuildRequest (Route "DELETE" path spec) fullParams body' response where
  buildRequest _ = buildRequest_ "DELETE" (SProxy :: _ path) (Proxy :: _ params) (Proxy :: _ query) (Proxy :: _ body)

buildRequest_ ::
  forall path pathParams queryParams params bodyRep body.
  IsSymbol path =>
  WriteParams pathParams queryParams params =>
  MediaType bodyRep =>
  EncodeMedia bodyRep body =>
  String ->
  SProxy path ->
  Proxy pathParams ->
  Proxy queryParams ->
  Proxy bodyRep ->
  params ->
  body ->
  Request
buildRequest_ method path pathParams queryParams bodyRep params body =
  { method: unsafeCoerce method
  , url: unsafeCoerce (writeParams pathParams queryParams params (reflectSymbol path))
  , headers: maybe Object.empty (Object.singleton "Content-Type" <<< unwrap) (mediaType bodyRep)
  , body: encodeMedia bodyRep body
  }
