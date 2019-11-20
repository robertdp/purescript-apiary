## Module Apiary.Media

#### `MediaCodec`

``` purescript
class MediaCodec rep a | rep -> a where
  mediaType :: Proxy rep -> Maybe MediaType
  encodeMedia :: Proxy rep -> a -> String
  decodeMedia :: Proxy rep -> String -> F a
```

##### Instances
``` purescript
MediaCodec Unit Unit
MediaCodec String String
(WriteForeign a, ReadForeign a) => MediaCodec (JSON a) a
```

