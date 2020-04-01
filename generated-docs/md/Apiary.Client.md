## Module Apiary.Client

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

### Re-exported from Apiary.Types:

#### `Response`

``` purescript
type Response = { body :: String, headers :: Headers, status :: Int }
```

#### `Request`

``` purescript
type Request = { body :: String, headers :: Headers, method :: Method, url :: URL }
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

