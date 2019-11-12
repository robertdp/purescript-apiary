## Module Apiary

#### `makeRequest`

``` purescript
makeRequest :: forall route params body rep response. BuildRequest route params body rep => DecodeResponse rep response => route -> (Request -> Request) -> params -> body -> Aff (Either Error response)
```

#### `lift`

``` purescript
lift :: Aff ~> (ExceptT Error Aff)
```

#### `fetch`

``` purescript
fetch :: Request -> ExceptT Error Aff Response
```


