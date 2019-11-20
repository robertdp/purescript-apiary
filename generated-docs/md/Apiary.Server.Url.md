## Module Apiary.Server.Url

#### `PathParams`

``` purescript
type PathParams = Object String
```

#### `QueryParams`

``` purescript
type QueryParams = Object Foreign
```

#### `ReadParams`

``` purescript
class ReadParams pathParams queryParams params | pathParams queryParams -> params where
  readParams :: Proxy pathParams -> Proxy queryParams -> PathParams -> QueryParams -> F params
```

##### Instances
``` purescript
(Union pathParams queryParams mergedParams, RowToList pathParams pathParamList, ReadPathParams pathParams pathParamList, RowToList queryParamsRep queryParamList, DecodeQueryParams queryParams queryParamList) => ReadParams (Record pathParams) (Record queryParamsRep) (Record mergedParams)
```

#### `coerceQuery`

``` purescript
coerceQuery :: Query -> QueryParams
```

#### `ReadPathParams`

``` purescript
class ReadPathParams (params :: # Type) (paramList :: RowList) | paramList -> params where
  readPathParams :: RLProxy paramList -> PathParams -> F (Builder (Record ()) (Record params))
```

##### Instances
``` purescript
ReadPathParams () Nil
(Cons name value params' params, ReadPathParams params' paramList, IsSymbol name, Lacks name params', UrlParam value) => ReadPathParams params (Cons name value paramList)
```

#### `DecodeQueryParams`

``` purescript
class DecodeQueryParams params paramList | paramList -> params where
  decodeQueryParams :: RLProxy paramList -> QueryParams -> F (Builder (Record ()) (Record params))
```

##### Instances
``` purescript
DecodeQueryParams () Nil
(IsSymbol name, UrlParam value, Cons name (Array value) params' params, Lacks name params', DecodeQueryParams params' paramList) => DecodeQueryParams params (Cons name (Array value) paramList)
(IsSymbol name, UrlParam value, Cons name (Maybe value) params' params, Lacks name params', DecodeQueryParams params' paramList) => DecodeQueryParams params (Cons name value paramList)
```


