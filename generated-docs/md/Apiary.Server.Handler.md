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


