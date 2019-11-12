## Module Apiary.Status

#### `Status`

``` purescript
newtype Status
  = Status { code :: Int, reason :: String }
```

#### `statusCode`

``` purescript
statusCode :: Status -> Int
```

#### `statusReason`

``` purescript
statusReason :: Status -> String
```

#### `status`

``` purescript
status :: Int -> String -> Status
```

#### `ok`

``` purescript
ok :: Status
```

#### `created`

``` purescript
created :: Status
```

#### `noContent`

``` purescript
noContent :: Status
```

#### `notModified`

``` purescript
notModified :: Status
```

#### `badRequest`

``` purescript
badRequest :: Status
```

#### `unauthorized`

``` purescript
unauthorized :: Status
```

#### `forbidden`

``` purescript
forbidden :: Status
```

#### `notFound`

``` purescript
notFound :: Status
```

#### `conflict`

``` purescript
conflict :: Status
```

#### `maintenanceInProgress`

``` purescript
maintenanceInProgress :: Status
```

#### `ResponseStatus`

``` purescript
class ResponseStatus (status :: Symbol)  where
  toStatus :: SProxy status -> Status
```

##### Instances
``` purescript
ResponseStatus "ok"
ResponseStatus "created"
ResponseStatus "noContent"
ResponseStatus "notModified"
ResponseStatus "badRequest"
ResponseStatus "unauthorized"
ResponseStatus "forbidden"
ResponseStatus "notFound"
ResponseStatus "conflict"
(Fail (Beside (Text "Unsupported response status: ") (Text status))) => ResponseStatus status
```


