module Apiary.Params where

import Prelude
import Data.Foldable (intercalate)
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), Replacement(..))
import Data.String as String
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Global.Unsafe (unsafeEncodeURIComponent)
import Prim.Row (class Cons, class Nub, class Union)
import Prim.RowList (kind RowList, class RowToList, Cons, Nil)
import Record as Record
import Type.Data.RowList (RLProxy(..))
import Type.Proxy (Proxy)
import Unsafe.Coerce (unsafeCoerce)

class EncodeParam param where
  encodeParam :: param -> String

instance encodeParamInt :: EncodeParam Int where
  encodeParam = show

instance encodeParamString :: EncodeParam String where
  encodeParam = unsafeEncodeURIComponent

class WriteParams pathParams queryParams params | pathParams queryParams -> params where
  writeParams :: Proxy pathParams -> Proxy queryParams -> params -> String -> String

instance writeParamsRecord ::
  ( Union pathParams queryParamRep mergedParams
  , Nub mergedParams cleanParams
  , RowToList pathParams pathParamList
  , RowToList queryParams queryParamList
  , WritePathParams pathParams pathParamList
  , BuildQueryParams queryParamRep queryParamList
  ) =>
  WriteParams (Record pathParams) (Record queryParams) (Record cleanParams) where
  writeParams _ _ params url = path <> prefix query
    where
    coercePathParams = unsafeCoerce :: Record cleanParams -> Record pathParams

    coerceQueryParams = unsafeCoerce :: Record cleanParams -> Record queryParamRep

    path = writePathParams (RLProxy :: _ pathParamList) (coercePathParams params) url

    query = intercalate "&" $ buildQueryParams (RLProxy :: _ queryParamList) (coerceQueryParams params)

    prefix q = if String.null q then "" else "?" <> q

class WritePathParams (params :: #Type) (paramList :: RowList) | paramList -> params where
  writePathParams :: RLProxy paramList -> Record params -> String -> String

instance writePathParamsNil :: WritePathParams () Nil where
  writePathParams _ _ = identity

instance writePathParamsCons ::
  ( IsSymbol name
  , EncodeParam value
  , Cons name value params' params
  , WritePathParams params' paramTail
  ) =>
  WritePathParams params (Cons name value paramTail) where
  writePathParams _ params url =
    writePathParams (RLProxy :: _ paramTail) (coerceParams params)
      $ String.replace (Pattern pattern) (Replacement replacement) url
    where
    coerceParams = unsafeCoerce :: Record params -> Record params'

    pattern = ":" <> reflectSymbol (SProxy :: _ name)

    replacement = encodeParam $ Record.get (SProxy :: _ name) params

class BuildQueryParams (params :: #Type) (paramList :: RowList) | paramList -> params where
  buildQueryParams :: RLProxy paramList -> Record params -> Array String

instance buildQueryParamsNil :: BuildQueryParams () Nil where
  buildQueryParams _ _ = []

instance buildQueryParamsConsArray ::
  ( IsSymbol name
  , EncodeParam value
  , Cons name (Array value) params' params
  , BuildQueryParams params' paramTail
  ) =>
  BuildQueryParams params (Cons name (Array value) paramTail) where
  buildQueryParams _ params = query <> buildQueryParams (RLProxy :: _ paramTail) (coerceParams params)
    where
    coerceParams = unsafeCoerce :: Record params -> Record params'

    name' = SProxy :: _ name

    name = reflectSymbol name'

    query = Record.get name' params <#> encodeQueryParam name
else instance buildQueryParamsCons ::
  ( IsSymbol name
  , EncodeParam value
  , Cons name (Maybe value) params' params
  , BuildQueryParams params' paramTail
  ) =>
  BuildQueryParams params (Cons name value paramTail) where
  buildQueryParams _ params = query <> buildQueryParams (RLProxy :: _ paramTail) (coerceParams params)
    where
    coerceParams = unsafeCoerce :: Record params -> Record params'

    name' = SProxy :: _ name

    name = reflectSymbol name'

    query =
      case Record.get name' params of
        Nothing -> []
        Just value -> [ encodeQueryParam name value ]

encodeQueryParam :: forall name value. EncodeParam name => EncodeParam value => name -> value -> String
encodeQueryParam name value = encodeParam name <> "=" <> encodeParam value
