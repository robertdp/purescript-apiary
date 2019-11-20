## Module Apiary.Url

#### `UrlParam`

``` purescript
class UrlParam a  where
  encodeUrlParam :: a -> String
  decodeUrlParam :: String -> F a
```

##### Instances
``` purescript
UrlParam String
UrlParam Int
UrlParam Number
```


