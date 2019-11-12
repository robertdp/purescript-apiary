## Module Apiary.Request

#### `BuildRequest`

``` purescript
class BuildRequest route params body rep | route -> params body rep where
  buildRequest :: route -> params -> body -> Request
```


