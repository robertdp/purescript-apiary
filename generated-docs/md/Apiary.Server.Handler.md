## Module Apiary.Server.Handler

#### `Handler`

``` purescript
newtype Handler m from to a
  = Handler (Response -> m a)
```

##### Instances
``` purescript
(Monad m) => IxFunctor (Handler m)
(Monad m) => IxApply (Handler m)
(Monad m) => IxApplicative (Handler m)
(Monad m) => IxBind (Handler m)
(Monad m) => IxMonad (Handler m)
(Monad m) => IxMonadTrans Handler
(Monad m) => Functor (Handler m x x)
(Monad m) => Apply (Handler m x x)
(Monad m) => Applicative (Handler m x x)
(Monad m) => Bind (Handler m x x)
(Monad m) => Monad (Handler m x x)
```

#### `runHandler`

``` purescript
runHandler :: forall m from to a. Handler m from to a -> Response -> m a
```


### Re-exported from Control.Monad.Indexed.Qualified:

#### `pure`

``` purescript
pure :: forall m a x. IxApplicative m => a -> m x x a
```

#### `map`

``` purescript
map :: forall f a b x y. IxFunctor f => (a -> b) -> f x y a -> f x y b
```

#### `discard`

``` purescript
discard :: forall m a b x y z. IxBind m => IxDiscard a => m x y a -> (a -> m y z b) -> m x z b
```

#### `bind`

``` purescript
bind :: forall m a b x y z. IxMonad m => m x y a -> (a -> m y z b) -> m x z b
```

#### `apply`

``` purescript
apply :: forall m a b x y z. IxApply m => m x y (a -> b) -> m y z a -> m x z b
```

### Re-exported from Control.Monad.Indexed.Trans.Qualified:

#### `lift`

``` purescript
lift :: forall m i a t. IxMonadTrans t => Monad m => m a -> t m i i a
```

