## Module Apiary.Client.Response

#### `DecodeResponse`

``` purescript
class DecodeResponse rep response | rep -> response where
  decodeResponse :: Proxy rep -> Response -> Apiary response
```

##### Instances
``` purescript
DecodeResponse Unit Unit
DecodeResponse String String
(RowToList responses responseList, DecodeResponseVariant result responseList) => DecodeResponse (Record responses) (Variant result)
```

#### `DecodeResponseVariant`

``` purescript
class DecodeResponseVariant (response :: # Type) (responseList :: RowList) | responseList -> response where
  decodeResponseVariant :: RLProxy responseList -> Response -> Apiary (Variant response)
```

##### Instances
``` purescript
DecodeResponseVariant () Nil
(IsSymbol status, ResponseStatus status, Cons status decoded variant' variant, DecodeResponseVariant variant' responseList, Union variant' a variant, DecodeMedia rep decoded) => DecodeResponseVariant variant (Cons status rep responseList)
```


