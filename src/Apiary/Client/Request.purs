module Apiary.Client.Request
  ( class BuildRequest
  , buildRequest
  ) where

import Prelude
import Affjax.RequestHeader (RequestHeader(..))
import Apiary.Client.Response (class DecodeResponse)
import Apiary.Client.Url (class BuildUrl, buildUrl)
import Apiary.Media (class EncodeMedia, class MediaType, encodeMedia, mediaType)
import Apiary.Method (class RequestMethod, toMethod)
import Apiary.Route (class PrepareSpec, Route)
import Apiary.Types (None, Request)
import Data.Array as Array
import Data.Maybe (maybe)
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Type.Proxy (Proxy(..))

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
  buildRequest _ = buildRequest_ (SProxy :: _ "GET") (SProxy :: _ path) (Proxy :: _ None)
else instance buildRequestRouteRest ::
  ( PrepareSpec
      spec
      { path :: params
      , query :: query
      , body :: body
      , response :: response
      }
  , RequestMethod method
  , BuildUrl params query
  , MediaType body
  , EncodeMedia body body'
  , DecodeResponse response response'
  , IsSymbol path
  ) =>
  BuildRequest (Route method path spec) params query body' response where
  buildRequest _ = buildRequest_ (SProxy :: _ method) (SProxy :: _ path) (Proxy :: _ body)

buildRequest_ ::
  forall method path params query proxy bodyRep body.
  RequestMethod method =>
  IsSymbol path =>
  BuildUrl params query =>
  MediaType bodyRep =>
  EncodeMedia bodyRep body =>
  SProxy method ->
  SProxy path ->
  proxy bodyRep ->
  params ->
  query ->
  body ->
  Request
buildRequest_ method path bodyRep params query body =
  { method: toMethod method
  , url: buildUrl params query (reflectSymbol path)
  , headers: maybe [] (Array.singleton <<< ContentType) (mediaType bodyRep)
  , body: encodeMedia bodyRep body
  }
