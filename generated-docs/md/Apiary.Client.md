## Module Apiary.Client

#### `makeRequest`

``` purescript
makeRequest :: forall route params body rep response. BuildRequest route params body rep => DecodeResponse rep response => route -> (Request -> Request) -> params -> body -> Aff (Either Error response)
```

#### `fetch`

``` purescript
fetch :: Request -> ExceptT Error Aff Response
```


### Re-exported from Apiary.Media:

#### `None`

``` purescript
data None :: Type
```

##### Instances
``` purescript
Semigroup None
Monoid None
MediaType None
EncodeMedia None None
DecodeMedia None None
```

#### `JSON`

``` purescript
data JSON a
  = JSON
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
  decodeMedia :: Proxy rep -> String -> F a
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
  encodeMedia :: Proxy rep -> a -> String
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
  mediaType :: Proxy rep -> Maybe MediaType
```

##### Instances
``` purescript
MediaType None
MediaType String
MediaType (JSON a)
```

#### `none`

``` purescript
none :: None
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

#### `Apiary`

``` purescript
type Apiary = Except Error
```

#### `emptyRequest`

``` purescript
emptyRequest :: Request
```

