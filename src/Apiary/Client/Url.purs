module Apiary.Client.Url where

import Prelude
import Apiary.Url as Url
import Control.Monad.ST (ST)
import Data.Array as Array
import Data.Array.ST (STArray)
import Data.Array.ST as STArray
import Data.Either (either)
import Data.Foldable (class Foldable, intercalate)
import Data.Maybe (Maybe(..))
import Data.String as String
import Data.String.Regex as Regex
import Data.String.Regex.Flags as RegexFlags
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Prim.Row (class Cons)
import Prim.RowList (kind RowList, class RowToList, Cons, Nil)
import Record as Record
import Type.Data.RowList (RLProxy(..))

class BuildUrl params query where
  buildUrl :: params -> query -> String -> String

instance buildUrlRecord ::
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

buildPath :: forall params paramList. RowToList params paramList => ReplacePathParams params paramList => { | params } -> String -> String
buildPath = replacePathParams (RLProxy :: _ paramList)

class ReplacePathParams (params :: #Type) (paramList :: RowList) | paramList -> params where
  replacePathParams :: RLProxy paramList -> Record params -> String -> String

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
    # map (\{ name, value } -> Url.encodeParam name <> "=" <> value)
    # intercalate "&"

class PrepareQueryParams (query :: #Type) (queryList :: RowList) | queryList -> query where
  prepareQueryParams ::
    RLProxy queryList ->
    Record query ->
    (forall h. ST h (STArray h { name :: String, value :: String })) ->
    Array { name :: String, value :: String }

instance prepareQueryParamsNil :: PrepareQueryParams params Nil where
  prepareQueryParams _ _ builder = STArray.run builder

instance prepareQueryParamsConsArray ::
  ( IsSymbol name
  , Url.EncodeParam value
  , Cons name (f value) query' query
  , Foldable f
  , PrepareQueryParams query queryTail
  ) =>
  PrepareQueryParams query (Cons name (f value) queryTail) where
  prepareQueryParams _ query builder = do
    prepareQueryParams (RLProxy :: _ queryTail) query do
      array <- builder
      _ <- STArray.pushAll values array
      pure array
    where
    name = SProxy :: _ name

    values =
      Record.get name query
        # Array.fromFoldable
        # map \value -> { name: reflectSymbol name, value: Url.encodeParam value }
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

    value = { name: reflectSymbol name, value: Url.encodeParam $ Record.get name query }
