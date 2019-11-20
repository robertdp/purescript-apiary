## Module Apiary.Server

#### `makeHandler`

``` purescript
makeHandler :: forall route params body responder m. AttachToRouter route => DecodeRequest route params body => BuildResponder route responder => route -> (Request params body -> responder -> FullHandler m) -> ReaderT (m Unit -> Aff Unit) Router Unit
```

#### `sendMultipleErrors`

``` purescript
sendMultipleErrors :: forall m. MonadEffect m => MultipleErrors -> FullHandler m
```


