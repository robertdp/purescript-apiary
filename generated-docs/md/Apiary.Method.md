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

#### `coerceMethod`

``` purescript
coerceMethod :: forall method. IsSymbol method => SProxy method -> Method
```


