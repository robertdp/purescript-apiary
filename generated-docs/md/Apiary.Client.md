## Module Apiary.Client

#### `makeRequest`

``` purescript
makeRequest :: forall route params query body rep response. BuildRequest route params query body rep => DecodeResponse rep response => route -> (Request -> Request) -> params -> query -> body -> Aff (Either Error response)
```

#### `fetch`

``` purescript
fetch :: Request -> ExceptT Error Aff Response
```


