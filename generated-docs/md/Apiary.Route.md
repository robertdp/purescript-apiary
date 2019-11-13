## Module Apiary.Route

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

#### `SpecDefaults`

``` purescript
type SpecDefaults = (body :: Unit, params :: Record (), query :: Record (), response :: Unit)
```

#### `PrepareSpec`

``` purescript
class PrepareSpec spec prepared | spec -> prepared
```

##### Instances
``` purescript
(Union spec SpecDefaults specWithDefaults, Nub specWithDefaults prepared) => PrepareSpec (Record spec) (Record prepared)
```

#### `GET`

``` purescript
type GET = Route "GET"
```

#### `PATCH`

``` purescript
type PATCH = Route "PATCH"
```

#### `POST`

``` purescript
type POST = Route "POST"
```

#### `PUT`

``` purescript
type PUT = Route "PUT"
```

#### `DELETE`

``` purescript
type DELETE = Route "DELETE"
```

#### `buildRequest_`

``` purescript
buildRequest_ :: forall path pathParams queryParams params bodyRep body. IsSymbol path => WriteParams pathParams queryParams params => MediaCodec bodyRep body => String -> SProxy path -> Proxy pathParams -> Proxy queryParams -> Proxy bodyRep -> params -> body -> Request
```


