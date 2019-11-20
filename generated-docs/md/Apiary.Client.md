## Module Apiary.Client

#### `makeRequest`

``` purescript
makeRequest :: forall route params body rep response. BuildRequest route params body rep => DecodeResponse rep response => route -> (Request -> Request) -> params -> body -> Aff (Either Error response)
```

#### `fetch`

``` purescript
fetch :: Request -> ExceptT Error Aff Response
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

#### `JSON`

``` purescript
newtype JSON a
  = JSON a
```

##### Instances
``` purescript
Newtype (JSON a) _
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

