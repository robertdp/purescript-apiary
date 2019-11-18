## Module Apiary.Url

#### `UrlParam`

``` purescript
class UrlParam a  where
  encodeUrlParam :: a -> String
  decodeUrlParam :: String -> F a
```

##### Instances
``` purescript
UrlParam String
UrlParam Int
UrlParam Number
```

#### `WriteParams`

``` purescript
class WriteParams pathParams queryParams params | pathParams queryParams -> params where
  writeParams :: Proxy pathParams -> Proxy queryParams -> params -> String -> String
```

##### Instances
``` purescript
(Union pathParams queryParamRep mergedParams, Nub mergedParams cleanParams, RowToList pathParams pathParamList, RowToList queryParams queryParamList, WritePathParams pathParams pathParamList, BuildQueryParams queryParamRep queryParamList) => WriteParams (Record pathParams) (Record queryParams) (Record cleanParams)
```

#### `WritePathParams`

``` purescript
class WritePathParams (params :: # Type) (paramList :: RowList) | paramList -> params where
  writePathParams :: RLProxy paramList -> Record params -> String -> String
```

##### Instances
``` purescript
WritePathParams () Nil
(IsSymbol name, UrlParam value, Cons name value params' params, WritePathParams params' paramTail) => WritePathParams params (Cons name value paramTail)
```

#### `BuildQueryParams`

``` purescript
class BuildQueryParams (params :: # Type) (paramList :: RowList) | paramList -> params where
  buildQueryParams :: RLProxy paramList -> Record params -> Array (Tuple String String)
```

##### Instances
``` purescript
BuildQueryParams () Nil
(IsSymbol name, UrlParam value, Cons name (Array value) params' params, BuildQueryParams params' paramTail) => BuildQueryParams params (Cons name (Array value) paramTail)
(IsSymbol name, UrlParam value, Cons name (Maybe value) params' params, BuildQueryParams params' paramTail) => BuildQueryParams params (Cons name value paramTail)
```


