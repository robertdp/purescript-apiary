## Module Apiary.Client.Request

#### `BuildRequest`

``` purescript
class BuildRequest route params query body rep | route -> params query body rep where
  buildRequest :: route -> params -> query -> body -> Request
```

##### Instances
``` purescript
(PrepareSpec spec { body :: None, path :: params, query :: query, response :: response }, BuildUrl params query, DecodeResponse response response', IsSymbol path) => BuildRequest (Route "GET" path spec) params query None response
(PrepareSpec spec { body :: body, path :: params, query :: query, response :: response }, BuildUrl params query, MediaType body, EncodeMedia body body', DecodeResponse response response', IsSymbol path) => BuildRequest (Route "PATCH" path spec) params query body' response
(PrepareSpec spec { body :: body, path :: params, query :: query, response :: response }, BuildUrl params query, MediaType body, EncodeMedia body body', DecodeResponse response response', IsSymbol path) => BuildRequest (Route "POST" path spec) params query body' response
(PrepareSpec spec { body :: body, path :: params, query :: query, response :: response }, BuildUrl params query, MediaType body, EncodeMedia body body', DecodeResponse response response', IsSymbol path) => BuildRequest (Route "PUT" path spec) params query body' response
(PrepareSpec spec { body :: body, path :: params, query :: query, response :: response }, BuildUrl params query, MediaType body, EncodeMedia body body', DecodeResponse response response', IsSymbol path) => BuildRequest (Route "DELETE" path spec) params query body' response
```


