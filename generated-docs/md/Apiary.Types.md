## Module Apiary.Types

#### `URL`

``` purescript
type URL = String
```

#### `Request`

``` purescript
type Request = { body :: String, headers :: Array RequestHeader, method :: Method, url :: URL }
```

#### `emptyRequest`

``` purescript
emptyRequest :: Request
```

#### `Response`

``` purescript
type Response = { body :: String, headers :: Array ResponseHeader, status :: StatusCode }
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

#### `none`

``` purescript
none :: None
```


