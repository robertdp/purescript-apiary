## Module Apiary.Request

#### `BuildRequest`

``` purescript
class BuildRequest route params query body rep | route -> params query body rep where
  buildRequest :: route -> params -> query -> body -> Request
```


