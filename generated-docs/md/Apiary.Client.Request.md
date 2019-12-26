## Module Apiary.Client.Request

#### `BuildRequest`

``` purescript
class BuildRequest route params body rep | route -> params body rep where
  buildRequest :: route -> params -> body -> Request
```

##### Instances
``` purescript
(PrepareSpec spec { body :: None, params :: params, query :: query, response :: response }, WriteParams params query fullParams, DecodeResponse response response', IsSymbol path) => BuildRequest (Route "GET" path spec) fullParams None response
(PrepareSpec spec { body :: body, params :: params, query :: query, response :: response }, WriteParams params query fullParams, MediaType body, EncodeMedia body body', DecodeResponse response response', IsSymbol path) => BuildRequest (Route "PATCH" path spec) fullParams body' response
(PrepareSpec spec { body :: body, params :: params, query :: query, response :: response }, WriteParams params query fullParams, MediaType body, EncodeMedia body body', DecodeResponse response response', IsSymbol path) => BuildRequest (Route "POST" path spec) fullParams body' response
(PrepareSpec spec { body :: body, params :: params, query :: query, response :: response }, WriteParams params query fullParams, MediaType body, EncodeMedia body body', DecodeResponse response response', IsSymbol path) => BuildRequest (Route "PUT" path spec) fullParams body' response
(PrepareSpec spec { body :: body, params :: params, query :: query, response :: response }, WriteParams params query fullParams, MediaType body, EncodeMedia body body', DecodeResponse response response', IsSymbol path) => BuildRequest (Route "DELETE" path spec) fullParams body' response
```


