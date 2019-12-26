## Module Apiary.Media

#### `MediaType`

``` purescript
class MediaType rep  where
  mediaType :: Proxy rep -> Maybe MediaType
```

##### Instances
``` purescript
MediaType None
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
EncodeMedia None None
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
DecodeMedia None None
DecodeMedia String String
(ReadForeign a) => DecodeMedia (JSON a) a
```

#### `None`

``` purescript
data None :: Type
```

##### Instances
``` purescript
Semigroup None
Monoid None
MediaType None
EncodeMedia None None
DecodeMedia None None
```

#### `none`

``` purescript
none :: None
```

#### `JSON`

``` purescript
data JSON a
  = JSON
```

##### Instances
``` purescript
MediaType (JSON a)
(WriteForeign a) => EncodeMedia (JSON a) a
(ReadForeign a) => DecodeMedia (JSON a) a
```


