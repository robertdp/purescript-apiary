module Apiary.Route where

import Prelude
import Apiary.Body (class EncodeBody, encodeBody)
import Apiary.Params (class WriteParams, writeParams)
import Apiary.Request (class BuildRequest)
import Apiary.Response (class DecodeResponse)
import Apiary.Types (Request)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Variant (SProxy(..))
import Foreign.Object as Object
import Prim.Row (class Nub, class Union)
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

data Route (method :: Symbol) (path :: Symbol) spec
  = Route

type SpecDefaults
  = ( params :: {}
    , query :: {}
    , body :: Unit
    , response :: Unit
    )

class PrepareSpec spec prepared | spec -> prepared

instance prepareSpec ::
  ( Union spec SpecDefaults specWithDefaults
  , Nub specWithDefaults prepared
  ) =>
  PrepareSpec (Record spec) (Record prepared)

type GET
  = Route "GET"

type PATCH
  = Route "PATCH"

type POST
  = Route "POST"

type PUT
  = Route "PUT"

type DELETE
  = Route "DELETE"

instance buildRequestRouteGET ::
  ( PrepareSpec
      spec
      { params :: params
      , query :: query
      , body :: Unit
      , response :: response
      }
  , WriteParams params query fullParams
  , DecodeResponse response response'
  , IsSymbol path
  ) =>
  BuildRequest (Route "GET" path spec) fullParams Unit response where
  buildRequest _ = buildRequest_ "GET" (SProxy :: _ path) (Proxy :: _ params) (Proxy :: _ query) (Proxy :: _ Unit)
else instance buildRequestRoutePATCH ::
  ( PrepareSpec
      spec
      { params :: params
      , query :: query
      , body :: body
      , response :: response
      }
  , WriteParams params query fullParams
  , EncodeBody body body'
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
  , EncodeBody body body'
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
  , EncodeBody body body'
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
  , EncodeBody body body'
  , DecodeResponse response response'
  , IsSymbol path
  ) =>
  BuildRequest (Route "DELETE" path spec) fullParams body' response where
  buildRequest _ = buildRequest_ "DELETE" (SProxy :: _ path) (Proxy :: _ params) (Proxy :: _ query) (Proxy :: _ body)

buildRequest_ ::
  forall path pathParams queryParams params bodyRep body.
  IsSymbol path =>
  WriteParams pathParams queryParams params =>
  EncodeBody bodyRep body =>
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
  , headers: Object.empty
  , body: encodeBody bodyRep body
  }
