## Module Apiary

#### `makeRequest`

``` purescript
makeRequest :: forall route params body rep response. BuildRequest route params body rep => DecodeResponse rep response => route -> (Request -> Request) -> params -> body -> Aff (Either Error response)
```


### Re-exported from Apiary.Route:

#### `Route`

``` purescript
data Route (method :: Symbol) (path :: Symbol) spec
  = Route
```

##### Instances
``` purescript
(PrepareSpec spec { body :: Unit, params :: params, query :: query, response :: response }, WriteParams params query fullParams, DecodeResponse response response', IsSymbol path) => BuildRequest (Route "GET" path spec) fullParams Unit response
(PrepareSpec spec { body :: body, params :: params, query :: query, response :: response }, WriteParams params query fullParams, MediaCodec body body', DecodeResponse response response', IsSymbol path) => BuildRequest (Route "PATCH" path spec) fullParams body' response
(PrepareSpec spec { body :: body, params :: params, query :: query, response :: response }, WriteParams params query fullParams, MediaCodec body body', DecodeResponse response response', IsSymbol path) => BuildRequest (Route "POST" path spec) fullParams body' response
(PrepareSpec spec { body :: body, params :: params, query :: query, response :: response }, WriteParams params query fullParams, MediaCodec body body', DecodeResponse response response', IsSymbol path) => BuildRequest (Route "PUT" path spec) fullParams body' response
(PrepareSpec spec { body :: body, params :: params, query :: query, response :: response }, WriteParams params query fullParams, MediaCodec body body', DecodeResponse response response', IsSymbol path) => BuildRequest (Route "DELETE" path spec) fullParams body' response
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

#### `Response`

``` purescript
type Response = { body :: String, headers :: Headers, status :: Int }
```

#### `Request`

``` purescript
type Request = { body :: String, headers :: Headers, method :: Method, url :: URL }
```

#### `Error`

``` purescript
data Error
  = RuntimeError Error
  | DecodeError MultipleErrors Response
  | UnexpectedResponse Response
```

##### Instances
``` purescript
Show Error
Semigroup Error
```

