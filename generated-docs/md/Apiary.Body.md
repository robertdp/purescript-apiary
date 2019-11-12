## Module Apiary.Body

#### `DecodeBody`

``` purescript
class DecodeBody rep a | rep -> a where
  decodeBody :: Proxy rep -> String -> F a
```

##### Instances
``` purescript
DecodeBody Unit Unit
DecodeBody String String
(ReadForeign a) => DecodeBody (JSON a) a
```

#### `JSON`

``` purescript
newtype JSON a
  = JSON a
```

##### Instances
``` purescript
(ReadForeign a) => DecodeBody (JSON a) a
(WriteForeign a) => EncodeBody (JSON a) a
```

#### `EncodeBody`

``` purescript
class EncodeBody rep a | rep -> a where
  encodeBody :: Proxy rep -> a -> String
```

##### Instances
``` purescript
EncodeBody Unit Unit
EncodeBody String String
(WriteForeign a) => EncodeBody (JSON a) a
```


