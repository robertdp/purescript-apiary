## Module Apiary.Method

#### `RequestMethod`

``` purescript
class RequestMethod (method :: Symbol)  where
  toMethod :: SProxy method -> Method
```

##### Instances
``` purescript
RequestMethod "GET"
RequestMethod "POST"
RequestMethod "PUT"
RequestMethod "PATCH"
RequestMethod "DELETE"
(Fail (Beside (Text "Unsupported request method: ") (Text method))) => RequestMethod method
```


### Re-exported from Data.HTTP.Method:

#### `Method`

``` purescript
data Method
  = OPTIONS
  | GET
  | HEAD
  | POST
  | PUT
  | DELETE
  | TRACE
  | CONNECT
  | PROPFIND
  | PROPPATCH
  | MKCOL
  | COPY
  | MOVE
  | LOCK
  | UNLOCK
  | PATCH
```

##### Instances
``` purescript
Eq Method
Ord Method
Show Method
```

