## Module Apiary.Types

#### `Request`

``` purescript
type Request = { body :: String, headers :: Headers, method :: Method, url :: URL }
```

#### `emptyRequest`

``` purescript
emptyRequest :: Request
```

#### `Response`

``` purescript
type Response = { body :: String, headers :: Headers, status :: Int }
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

#### `showRequest`

``` purescript
showRequest :: Request -> String
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


