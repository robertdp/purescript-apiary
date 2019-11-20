## Module Apiary.Server.Request

#### `Request`

``` purescript
type Request params body = { body :: body, headers :: Object String, params :: params }
```

#### `DecodeRequest`

``` purescript
class DecodeRequest route params body | route -> params body where
  decodeRequest :: route -> PathParams -> QueryParams -> String -> F (Request params body)
```

##### Instances
``` purescript
(PrepareSpec spec { body :: body, params :: params, query :: query, response :: response }, ReadParams params query fullParams, MediaCodec body body') => DecodeRequest (Route path method spec) fullParams body'
```

#### `readBodyAsBuffer`

``` purescript
readBodyAsBuffer :: Request -> Aff Buffer
```

#### `readBodyAsString`

``` purescript
readBodyAsString :: Request -> Aff String
```

#### `requestQuery`

``` purescript
requestQuery :: Request -> QueryParams
```

#### `coerceQuery`

``` purescript
coerceQuery :: Query -> QueryParams
```


