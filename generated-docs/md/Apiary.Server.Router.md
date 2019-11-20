## Module Apiary.Server.Router

#### `Router`

``` purescript
newtype Router a
  = Router (RouterInstance -> Effect a)
```

##### Instances
``` purescript
Functor Router
Apply Router
Applicative Router
Bind Router
Monad Router
MonadEffect Router
```

#### `RouterInstance`

``` purescript
data RouterInstance :: Type
```

#### `_create`

``` purescript
_create :: EffectFn1 (EffectFn2 Request Response Unit) RouterInstance
```

#### `create`

``` purescript
create :: (Request -> Response -> Effect Unit) -> Effect RouterInstance
```

#### `_lookup`

``` purescript
_lookup :: EffectFn3 RouterInstance Request Response Unit
```

#### `lookup`

``` purescript
lookup :: Request -> Response -> RouterInstance -> Effect Unit
```

#### `createServer`

``` purescript
createServer :: forall m. MonadEffect m => (Request -> Response -> Effect Unit) -> Router Unit -> m Server
```

#### `_on`

``` purescript
_on :: EffectFn4 RouterInstance String String (EffectFn3 Request Response PathParams Unit) Unit
```

#### `Method`

``` purescript
type Method = String
```

#### `Path`

``` purescript
type Path = String
```

#### `on`

``` purescript
on :: Method -> Path -> (Request -> Response -> PathParams -> Effect Unit) -> Router Unit
```

#### `AttachToRouter`

``` purescript
class AttachToRouter route  where
  attachToRouter :: route -> (Request -> Response -> PathParams -> Effect Unit) -> Router Unit
```

##### Instances
``` purescript
(IsSymbol method, IsSymbol path) => AttachToRouter (Route method path spec)
```


