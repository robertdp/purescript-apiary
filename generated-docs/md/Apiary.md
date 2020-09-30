## Module Apiary


### Re-exported from Affjax.RequestHeader:

#### `RequestHeader`

``` purescript
data RequestHeader
  = Accept MediaType
  | ContentType MediaType
  | RequestHeader String String
```

##### Instances
``` purescript
Eq RequestHeader
Ord RequestHeader
Show RequestHeader
```

### Re-exported from Apiary.Client:

#### `makeRequest`

``` purescript
makeRequest :: forall route params query body rep response. BuildRequest route params query body rep => DecodeResponse rep response => route -> (Request -> Request) -> params -> query -> body -> Aff (Either Error response)
```

#### `fetch`

``` purescript
fetch :: Request -> ExceptT Error Aff Response
```

### Re-exported from Apiary.Media:

#### `JSON`

``` purescript
data JSON a
```

##### Instances
``` purescript
MediaType (JSON a)
(WriteForeign a) => EncodeMedia (JSON a) a
(ReadForeign a) => DecodeMedia (JSON a) a
```

#### `DecodeMedia`

``` purescript
class DecodeMedia rep a | rep -> a where
  decodeMedia :: forall proxy. proxy rep -> String -> F a
```

##### Instances
``` purescript
DecodeMedia None None
DecodeMedia String String
(ReadForeign a) => DecodeMedia (JSON a) a
```

#### `EncodeMedia`

``` purescript
class EncodeMedia rep a | rep -> a where
  encodeMedia :: forall proxy. proxy rep -> a -> String
```

##### Instances
``` purescript
EncodeMedia None None
EncodeMedia String String
(WriteForeign a) => EncodeMedia (JSON a) a
```

#### `MediaType`

``` purescript
class MediaType rep  where
  mediaType :: forall proxy. proxy rep -> Maybe MediaType
```

##### Instances
``` purescript
MediaType None
MediaType String
MediaType (JSON a)
```

### Re-exported from Apiary.Request:

#### `BuildRequest`

``` purescript
class BuildRequest route params query body rep | route -> params query body rep where
  buildRequest :: route -> params -> query -> body -> Request
```

##### Instances
``` purescript
(PrepareSpec spec { body :: None, path :: params, query :: query, response :: response }, BuildUrl params query, DecodeResponse response response', IsSymbol path) => BuildRequest (Route "GET" path spec) params query None response
(PrepareSpec spec { body :: body, path :: params, query :: query, response :: response }, RequestMethod method, BuildUrl params query, MediaType body, EncodeMedia body body', DecodeResponse response response', IsSymbol path) => BuildRequest (Route method path spec) params query body' response
```

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

#### `PrepareQueryParams`

``` purescript
class PrepareQueryParams (query :: # Type) (queryList :: RowList) | queryList -> query
```

##### Instances
``` purescript
PrepareQueryParams params Nil
(IsSymbol name, EncodeParam value, Cons name (f value) query' query, Foldable f, PrepareQueryParams query queryTail) => PrepareQueryParams query (Cons name (f value) queryTail)
(IsSymbol name, EncodeParam value, Cons name value query' query, PrepareQueryParams query queryTail) => PrepareQueryParams query (Cons name value queryTail)
```

#### `ReplacePathParams`

``` purescript
class ReplacePathParams (params :: # Type) (paramList :: RowList) | paramList -> params
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

#### `buildPath`

``` purescript
buildPath :: forall params paramList. RowToList params paramList => ReplacePathParams params paramList => Record params -> String -> String
```

### Re-exported from Apiary.Response:

#### `DecodeResponse`

``` purescript
class DecodeResponse rep response | rep -> response where
  decodeResponse :: forall proxy. proxy rep -> Response -> Except (Request -> Error) response
```

##### Instances
``` purescript
DecodeResponse None None
DecodeResponse String String
(RowToList responses responseList, DecodeResponseVariant result responseList) => DecodeResponse (Record responses) (Variant result)
```

#### `toStatus`

``` purescript
toStatus :: forall response responseList. RowToList response responseList => ResponseVariantToStatus response responseList => Variant response -> Status
```

### Re-exported from Apiary.Route:

#### `Route`

``` purescript
data Route (method :: Symbol) (path :: Symbol) spec
  = Route
```

#### `PUT`

``` purescript
type PUT = Route "PUT"
```

#### `POST`

``` purescript
type POST = Route "POST"
```

#### `PATCH`

``` purescript
type PATCH = Route "PATCH"
```

#### `GET`

``` purescript
type GET = Route "GET"
```

#### `DELETE`

``` purescript
type DELETE = Route "DELETE"
```

### Re-exported from Apiary.Types:

#### `URL`

``` purescript
type URL = String
```

#### `Response`

``` purescript
type Response = { body :: String, headers :: Array ResponseHeader, status :: StatusCode }
```

#### `Request`

``` purescript
type Request = { body :: String, headers :: Array RequestHeader, method :: Method, url :: URL }
```

#### `None`

``` purescript
data None :: Type
```

##### Instances
``` purescript
Show None
Semigroup None
Monoid None
```

#### `Error`

``` purescript
data Error
  = RuntimeError Error
  | DecodeError Request Response MultipleErrors
  | UnexpectedResponse Request Response
```

##### Instances
``` purescript
Show Error
Semigroup Error
```

#### `none`

``` purescript
none :: None
```

#### `emptyRequest`

``` purescript
emptyRequest :: Request
```

### Re-exported from Apiary.Url:

#### `DecodeParam`

``` purescript
class DecodeParam a  where
  decodeParam :: String -> F a
```

##### Instances
``` purescript
DecodeParam String
DecodeParam Boolean
DecodeParam Int
DecodeParam Number
```

#### `EncodeParam`

``` purescript
class EncodeParam a  where
  encodeParam :: a -> String
```

##### Instances
``` purescript
EncodeParam String
EncodeParam Boolean
EncodeParam Int
EncodeParam Number
```

