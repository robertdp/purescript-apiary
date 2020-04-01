## Module Apiary.Media

#### `DecodeMedia`

``` purescript
class DecodeMedia rep a | rep -> a where
  decodeMedia :: forall proxy. proxy rep -> String -> F a
```

##### Instances
``` purescript
DecodeMedia None None
DecodeMedia String String
(ReadForeign a) => DecodeMedia (JSON a) a
```

#### `EncodeMedia`

``` purescript
class EncodeMedia rep a | rep -> a where
  encodeMedia :: forall proxy. proxy rep -> a -> String
```

##### Instances
``` purescript
EncodeMedia None None
EncodeMedia String String
(WriteForeign a) => EncodeMedia (JSON a) a
```

#### `MediaType`

``` purescript
class MediaType rep  where
  mediaType :: forall proxy. proxy rep -> Maybe MediaType
```

##### Instances
``` purescript
MediaType None
MediaType String
MediaType (JSON a)
```

#### `JSON`

``` purescript
data JSON a
```

##### Instances
``` purescript
MediaType (JSON a)
(WriteForeign a) => EncodeMedia (JSON a) a
(ReadForeign a) => DecodeMedia (JSON a) a
```


