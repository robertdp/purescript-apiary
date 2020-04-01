## Module Apiary.Client.Url

#### `BuildUrl`

``` purescript
class BuildUrl params query  where
  buildUrl :: params -> query -> String -> String
```

##### Instances
``` purescript
(RowToList pathParams pathParamList, RowToList queryParams queryParamList, ReplacePathParams pathParams pathParamList, PrepareQueryParams queryParams queryParamList) => BuildUrl (Record pathParams) (Record queryParams)
(BuildUrl path (Record ())) => BuildUrl path None
(BuildUrl (Record ()) query) => BuildUrl None query
BuildUrl None None
```

#### `buildPath`

``` purescript
buildPath :: forall params paramList. RowToList params paramList => ReplacePathParams params paramList => Record params -> String -> String
```

#### `ReplacePathParams`

``` purescript
class ReplacePathParams (params :: # Type) (paramList :: RowList) | paramList -> params where
  replacePathParams :: forall proxy. proxy paramList -> Record params -> String -> String
```

##### Instances
``` purescript
ReplacePathParams params Nil
(IsSymbol name, EncodeParam value, Cons name value params' params, ReplacePathParams params paramTail) => ReplacePathParams params (Cons name value paramTail)
```

#### `buildQuery`

``` purescript
buildQuery :: forall query queryList. RowToList query queryList => PrepareQueryParams query queryList => Record query -> String
```

#### `PrepareQueryParams`

``` purescript
class PrepareQueryParams (query :: # Type) (queryList :: RowList) | queryList -> query where
  prepareQueryParams :: forall proxy. proxy queryList -> Record query -> (forall h. ST h (STArray h { name :: String, value :: String })) -> Array { name :: String, value :: String }
```

##### Instances
``` purescript
PrepareQueryParams params Nil
(IsSymbol name, EncodeParam value, Cons name (f value) query' query, Foldable f, PrepareQueryParams query queryTail) => PrepareQueryParams query (Cons name (f value) queryTail)
(IsSymbol name, EncodeParam value, Cons name value query' query, PrepareQueryParams query queryTail) => PrepareQueryParams query (Cons name value queryTail)
```


