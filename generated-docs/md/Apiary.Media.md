## Module Apiary.Media

#### `MediaType`

``` purescript
class MediaType rep  where
  mediaType :: Proxy rep -> Maybe MediaType
```

##### Instances
``` purescript
MediaType Unit
MediaType String
MediaType (JSON a)
```

#### `EncodeMedia`

``` purescript
class EncodeMedia rep a | rep -> a where
  encodeMedia :: Proxy rep -> a -> String
```

##### Instances
``` purescript
EncodeMedia Unit Unit
EncodeMedia String String
(WriteForeign a) => EncodeMedia (JSON a) a
```

#### `DecodeMedia`

``` purescript
class DecodeMedia rep a | rep -> a where
  decodeMedia :: Proxy rep -> String -> F a
```

##### Instances
``` purescript
DecodeMedia Unit Unit
DecodeMedia String String
(ReadForeign a) => DecodeMedia (JSON a) a
```

#### `JSON`

``` purescript
newtype JSON a
  = JSON a
```

##### Instances
``` purescript
MediaType (JSON a)
(WriteForeign a) => EncodeMedia (JSON a) a
(ReadForeign a) => DecodeMedia (JSON a) a
```


