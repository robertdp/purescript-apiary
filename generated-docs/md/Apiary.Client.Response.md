## Module Apiary.Client.Response

#### `DecodeResponse`

``` purescript
class DecodeResponse rep response | rep -> response where
  decodeResponse :: Proxy rep -> Response -> Apiary response
```

##### Instances
``` purescript
DecodeResponse None None
DecodeResponse String String
```


