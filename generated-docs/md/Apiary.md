## Module Apiary

#### `makeRequest`

``` purescript
makeRequest :: forall route params body rep response. BuildRequest route params body rep => DecodeResponse rep response => route -> (Request -> Request) -> params -> body -> Aff (Either Error response)
```

#### `lift`

``` purescript
lift :: Aff ~> (ExceptT Error Aff)
```

#### `fetch`

``` purescript
fetch :: Request -> ExceptT Error Aff Response
```


### Re-exported from Apiary.Request:

#### `BuildRequest`

``` purescript
class BuildRequest route params body rep | route -> params body rep where
  buildRequest :: route -> params -> body -> Request
```

### Re-exported from Apiary.Response:

#### `DecodeResponse`

``` purescript
class DecodeResponse rep response | rep -> response where
  decodeResponse :: Proxy rep -> Response -> Apiary response
```

##### Instances
``` purescript
DecodeResponse Unit Unit
DecodeResponse String String
(RowToList responses responseList, DecodeResponseVariant result responseList) => DecodeResponse (Record responses) (Variant result)
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
  | DecodeError Response MultipleErrors
  | UnexpectedResponse Response
```

##### Instances
``` purescript
Semigroup Error
```

