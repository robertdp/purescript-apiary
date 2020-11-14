module Apiary.Request
  ( class BuildRequest
  , class BuildUrl
  , class PrepareQueryParams
  , class ReplacePathParams
  , buildPath
  , buildQuery
  , buildRequest
  , buildUrl
  , prepareQueryParams
  , replacePathParams
  ) where

import Prelude
import Affjax.RequestHeader (RequestHeader(..))
import Apiary.Media (class EncodeMedia, class MediaType, encodeMedia, mediaType)
import Apiary.Method (class RequestMethod, toMethod)
import Apiary.Response (class DecodeResponse)
import Apiary.Route (class PrepareSpec, Route)
import Apiary.Types (None, Request)
import Apiary.Url as Url
import Control.Monad.ST (ST)
import Data.Array (intercalate)
import Data.Array as Array
import Data.Array.ST as STArray
import Data.Either (either)
import Data.Maybe (Maybe, maybe)
import Data.String as String
import Data.String.Regex as Regex
import Data.String.Regex.Flags as RegexFlags
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Prim.Row (class Cons)
import Prim.RowList (class RowToList, Cons, Nil, kind RowList)
import Record as Record
import Type.Data.RowList (RLProxy(..))
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

class BuildUrl params query where
  buildUrl :: params -> query -> String -> String

instance buildUrlPathAndQuery ::
  ( RowToList pathParams pathParamList
  , RowToList queryParams queryParamList
  , ReplacePathParams pathParams pathParamList
  , PrepareQueryParams queryParams queryParamList
  ) =>
  BuildUrl { | pathParams } { | queryParams } where
  buildUrl pathParams queryParams url = path <> prefix query
    where
    path = buildPath pathParams url

    query = buildQuery queryParams

    prefix q = if String.null q then "" else "?" <> q
else instance buildUrlPathOnly ::
  (BuildUrl path {}) =>
  BuildUrl path None where
  buildUrl pathParams _ url = buildUrl pathParams {} url
else instance buildUrlQueryOnly ::
  (BuildUrl {} query) =>
  BuildUrl None query where
  buildUrl _ queryParams url = buildUrl {} queryParams url
else instance buildUrlNone ::
  BuildUrl None None where
  buildUrl _ _ url = buildUrl {} {} url

buildPath :: forall params paramList. RowToList params paramList => ReplacePathParams params paramList => { | params } -> String -> String
buildPath = replacePathParams (RLProxy :: _ paramList)

class ReplacePathParams (params :: # Type) (paramList :: RowList) | paramList -> params where
  replacePathParams :: forall proxy. proxy paramList -> Record params -> String -> String

instance replacePathParamsNil :: ReplacePathParams params Nil where
  replacePathParams _ _ = identity

instance replacePathParamsCons ::
  ( IsSymbol name
  , Url.EncodeParam value
  , Cons name value params' params
  , ReplacePathParams params paramTail
  ) =>
  ReplacePathParams params (Cons name value paramTail) where
  replacePathParams _ params url = replacePathParams (RLProxy :: _ paramTail) params replaced
    where
    regex = Regex.regex ("\\:" <> reflectSymbol (SProxy :: _ name) <> "\\b") RegexFlags.global

    replaced = either (const url) (\pattern -> Regex.replace pattern replacement url) regex

    replacement = Url.encodeParam $ Record.get (SProxy :: _ name) params

buildQuery :: forall query queryList. RowToList query queryList => PrepareQueryParams query queryList => { | query } -> String
buildQuery query =
  prepareQueryParams (RLProxy :: _ queryList) query STArray.empty
    # intercalate "&"

class PrepareQueryParams (query :: # Type) (queryList :: RowList) | queryList -> query where
  prepareQueryParams ::
    forall proxy.
    proxy queryList ->
    Record query ->
    (forall h. ST h (STArray.STArray h String)) ->
    Array String

instance prepareQueryParamsNil :: PrepareQueryParams params Nil where
  prepareQueryParams _ _ builder = STArray.run builder

instance prepareQueryParamsConsArray ::
  ( IsSymbol name
  , Url.EncodeParam value
  , Cons name (Maybe value) query' query
  , PrepareQueryParams query queryTail
  ) =>
  PrepareQueryParams query (Cons name (Maybe value) queryTail) where
  prepareQueryParams _ query builder = do
    prepareQueryParams (RLProxy :: _ queryTail) query do
      array <- builder
      _ <- STArray.pushAll values array
      pure array
    where
    name = SProxy :: _ name

    values =
      Record.get name query
        # maybe [] \value ->
            [ Url.encodeParam (reflectSymbol name) <> "=" <> Url.encodeParam value ]
else instance prepareQueryParamsCons ::
  ( IsSymbol name
  , Url.EncodeParam value
  , Cons name value query' query
  , PrepareQueryParams query queryTail
  ) =>
  PrepareQueryParams query (Cons name value queryTail) where
  prepareQueryParams _ query builder = do
    prepareQueryParams (RLProxy :: _ queryTail) query do
      array <- builder
      _ <- STArray.push value array
      pure array
    where
    name = SProxy :: _ name

    value = Url.encodeParam (reflectSymbol name) <> "=" <> Url.encodeParam (Record.get name query)
