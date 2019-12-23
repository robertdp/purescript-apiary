## Module Apiary.Client.Request

#### `BuildRequest`

``` purescript
class BuildRequest route params body rep | route -> params body rep where
  buildRequest :: route -> params -> body -> Request
```

##### Instances
``` purescript
(PrepareSpec spec { body :: Unit, params :: params, query :: query, response :: response }, WriteParams params query fullParams, DecodeResponse response response', IsSymbol path) => BuildRequest (Route "GET" path spec) fullParams Unit response
(PrepareSpec spec { body :: body, params :: params, query :: query, response :: response }, WriteParams params query fullParams, MediaType body, EncodeMedia body body', DecodeResponse response response', IsSymbol path) => BuildRequest (Route "PATCH" path spec) fullParams body' response
(PrepareSpec spec { body :: body, params :: params, query :: query, response :: response }, WriteParams params query fullParams, MediaType body, EncodeMedia body body', DecodeResponse response response', IsSymbol path) => BuildRequest (Route "POST" path spec) fullParams body' response
(PrepareSpec spec { body :: body, params :: params, query :: query, response :: response }, WriteParams params query fullParams, MediaType body, EncodeMedia body body', DecodeResponse response response', IsSymbol path) => BuildRequest (Route "PUT" path spec) fullParams body' response
(PrepareSpec spec { body :: body, params :: params, query :: query, response :: response }, WriteParams params query fullParams, MediaType body, EncodeMedia body body', DecodeResponse response response', IsSymbol path) => BuildRequest (Route "DELETE" path spec) fullParams body' response
```

#### `buildRequest_`

``` purescript
buildRequest_ :: forall path pathParams queryParams params bodyRep body. IsSymbol path => WriteParams pathParams queryParams params => MediaType bodyRep => EncodeMedia bodyRep body => String -> SProxy path -> Proxy pathParams -> Proxy queryParams -> Proxy bodyRep -> params -> body -> Request
```


