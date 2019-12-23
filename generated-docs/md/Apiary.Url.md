## Module Apiary.Url

#### `EncodeParam`

``` purescript
class EncodeParam a  where
  encodeParam :: a -> String
```

##### Instances
``` purescript
EncodeParam String
EncodeParam Boolean
EncodeParam Int
EncodeParam Number
```

#### `DecodeParam`

``` purescript
class DecodeParam a  where
  decodeParam :: String -> F a
```

##### Instances
``` purescript
DecodeParam String
DecodeParam Boolean
DecodeParam Int
DecodeParam Number
```


