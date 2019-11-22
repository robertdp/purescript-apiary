module Apiary.Client.Url where

import Prelude
import Apiary.Url (class UrlParam, encodeUrlParam)
import Data.Either (either)
import Data.Foldable (intercalate)
import Data.Maybe (Maybe, maybe)
import Data.String as String
import Data.String.Regex as Regex
import Data.String.Regex.Flags as RegexFlags
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Data.Tuple (Tuple(..), fst, snd)
import Prim.Row (class Cons, class Nub, class Union)
import Prim.RowList (kind RowList, class RowToList, Cons, Nil)
import Record as Record
import Type.Data.RowList (RLProxy(..))
import Type.Proxy (Proxy)
import Unsafe.Coerce (unsafeCoerce)

class WriteParams pathParams queryParams params | pathParams queryParams -> params where
  writeParams :: Proxy pathParams -> Proxy queryParams -> params -> String -> String

instance writeParamsRecord ::
  ( Union pathParams queryParamRep dirtyParams
  , Nub dirtyParams mergedParams
  , RowToList pathParams pathParamList
  , RowToList queryParams queryParamList
  , WritePathParams pathParams pathParamList
  , BuildQueryParams queryParamRep queryParamList
  ) =>
  WriteParams (Record pathParams) (Record queryParams) (Record mergedParams) where
  writeParams _ _ params url = path <> prefix query
    where
    coercePathParams = unsafeCoerce :: Record mergedParams -> Record pathParams

    coerceQueryParams = unsafeCoerce :: Record mergedParams -> Record queryParamRep

    path = writePathParams (RLProxy :: _ pathParamList) (coercePathParams params) url

    query =
      buildQueryParams (RLProxy :: _ queryParamList) (coerceQueryParams params)
        # map (\param -> fst param <> "=" <> snd param)
        # intercalate "&"

    prefix q = if String.null q then "" else "?" <> q

class WritePathParams (params :: #Type) (paramList :: RowList) | paramList -> params where
  writePathParams :: RLProxy paramList -> Record params -> String -> String

instance writePathParamsNil :: WritePathParams () Nil where
  writePathParams _ _ = identity

instance writePathParamsCons ::
  ( IsSymbol name
  , UrlParam value
  , Cons name value params' params
  , WritePathParams params' paramTail
  ) =>
  WritePathParams params (Cons name value paramTail) where
  writePathParams _ params url = writePathParams (RLProxy :: _ paramTail) (coerceParams params) replaced
    where
    coerceParams = unsafeCoerce :: Record params -> Record params'

    regex = Regex.regex ("\\:" <> reflectSymbol (SProxy :: _ name) <> "\\b") RegexFlags.global

    replaced = either (const url) (\pattern -> Regex.replace pattern replacement url) regex

    replacement = encodeUrlParam $ Record.get (SProxy :: _ name) params

class BuildQueryParams (params :: #Type) (paramList :: RowList) | paramList -> params where
  buildQueryParams :: RLProxy paramList -> Record params -> Array (Tuple String String)

instance buildQueryParamsNil :: BuildQueryParams () Nil where
  buildQueryParams _ _ = []

instance buildQueryParamsConsArray ::
  ( IsSymbol name
  , UrlParam value
  , Cons name (Array value) params' params
  , BuildQueryParams params' paramTail
  ) =>
  BuildQueryParams params (Cons name (Array value) paramTail) where
  buildQueryParams _ params = query <> buildQueryParams (RLProxy :: _ paramTail) (coerceParams params)
    where
    coerceParams = unsafeCoerce :: Record params -> Record params'

    name' = SProxy :: _ name

    name = encodeUrlParam $ reflectSymbol name'

    query = (Tuple name <<< encodeUrlParam) <$> Record.get name' params
else instance buildQueryParamsCons ::
  ( IsSymbol name
  , UrlParam value
  , Cons name (Maybe value) params' params
  , BuildQueryParams params' paramTail
  ) =>
  BuildQueryParams params (Cons name value paramTail) where
  buildQueryParams _ params = query <> buildQueryParams (RLProxy :: _ paramTail) (coerceParams params)
    where
    coerceParams = unsafeCoerce :: Record params -> Record params'

    name' = SProxy :: _ name

    name = encodeUrlParam $ reflectSymbol name'

    query = maybe [] (pure <<< Tuple name <<< encodeUrlParam) $ Record.get name' params
