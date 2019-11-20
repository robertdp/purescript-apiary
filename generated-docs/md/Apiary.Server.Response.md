## Module Apiary.Server.Response

#### `StatusLineOpen`

``` purescript
data StatusLineOpen
```

#### `HeadersOpen`

``` purescript
data HeadersOpen
```

#### `BodyOpen`

``` purescript
data BodyOpen
```

#### `ResponseEnded`

``` purescript
data ResponseEnded
```

#### `Header`

``` purescript
type Header = Tuple String String
```

#### `FullHandler`

``` purescript
type FullHandler m = Handler m StatusLineOpen ResponseEnded Unit
```

#### `writeStatus`

``` purescript
writeStatus :: forall m. MonadEffect m => Status -> Handler m StatusLineOpen HeadersOpen Unit
```

#### `writeHeader`

``` purescript
writeHeader :: forall m. MonadEffect m => String -> String -> Handler m HeadersOpen HeadersOpen Unit
```

#### `closeHeaders`

``` purescript
closeHeaders :: forall m. Monad m => Handler m HeadersOpen BodyOpen Unit
```

#### `headers`

``` purescript
headers :: forall f m. Foldable f => MonadEffect m => f Header -> Handler m HeadersOpen BodyOpen Unit
```

#### `contentType`

``` purescript
contentType :: forall m. MonadEffect m => MediaType -> Handler m HeadersOpen HeadersOpen Unit
```

#### `withResponseStream`

``` purescript
withResponseStream :: forall m a. MonadEffect m => (Writable () -> m a) -> Handler m BodyOpen ResponseEnded a
```

#### `send`

``` purescript
send :: forall m. MonadEffect m => String -> Handler m BodyOpen ResponseEnded Unit
```

#### `respondWithMedia`

``` purescript
respondWithMedia :: forall m rep a. MediaCodec rep a => MonadEffect m => Status -> Proxy rep -> a -> FullHandler m
```

#### `BuildResponder`

``` purescript
class BuildResponder route responder | route -> responder where
  buildResponder :: route -> responder
```

##### Instances
``` purescript
(PrepareSpec spec { body :: body, params :: params, query :: query, response :: Record responses }, RowToList responses responseList, MonadEffect m, BuildResponderRecord responseList m responders) => BuildResponder (Route method path spec) (Record responders)
```

#### `BuildResponderRecord`

``` purescript
class BuildResponderRecord (responses :: RowList) (m :: Type -> Type) (responders :: # Type) | responses m -> responders where
  buildResponderRecord :: RLProxy responses -> Proxy2 m -> Builder (Record ()) (Record responders)
```

##### Instances
``` purescript
BuildResponderRecord Nil m ()
(IsSymbol status, ResponseStatus status, MediaCodec responseRep response, MonadEffect m, Lacks status responders', Cons status (response -> FullHandler m) responders' responders, BuildResponderRecord responseList m responders') => BuildResponderRecord (Cons status responseRep responseList) m responders
```


