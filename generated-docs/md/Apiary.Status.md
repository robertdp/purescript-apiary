## Module Apiary.Status

#### `Status`

``` purescript
newtype Status
  = Status { code :: Int, reason :: String }
```

##### Instances
``` purescript
Eq Status
Show Status
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

#### `_ok`

``` purescript
_ok :: SProxy "ok"
```

#### `created`

``` purescript
created :: Status
```

#### `_created`

``` purescript
_created :: SProxy "created"
```

#### `noContent`

``` purescript
noContent :: Status
```

#### `_noContent`

``` purescript
_noContent :: SProxy "noContent"
```

#### `notModified`

``` purescript
notModified :: Status
```

#### `_notModified`

``` purescript
_notModified :: SProxy "notModified"
```

#### `badRequest`

``` purescript
badRequest :: Status
```

#### `_badRequest`

``` purescript
_badRequest :: SProxy "badRequest"
```

#### `unauthorized`

``` purescript
unauthorized :: Status
```

#### `_unauthorized`

``` purescript
_unauthorized :: SProxy "unauthorized"
```

#### `forbidden`

``` purescript
forbidden :: Status
```

#### `_forbidden`

``` purescript
_forbidden :: SProxy "forbidden"
```

#### `notFound`

``` purescript
notFound :: Status
```

#### `_notFound`

``` purescript
_notFound :: SProxy "notFound"
```

#### `conflict`

``` purescript
conflict :: Status
```

#### `_conflict`

``` purescript
_conflict :: SProxy "conflict"
```

#### `maintenanceInProgress`

``` purescript
maintenanceInProgress :: Status
```

#### `_maintenanceInProgress`

``` purescript
_maintenanceInProgress :: SProxy "maintenanceInProgress"
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


