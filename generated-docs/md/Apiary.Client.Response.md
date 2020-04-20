## Module Apiary.Client.Response

#### `extract`

``` purescript
extract :: forall status r a. IsSymbol status => ResponseStatus status => Cons status a () r => Variant r -> a
```

#### `DecodeResponse`

``` purescript
class DecodeResponse rep response | rep -> response where
  decodeResponse :: forall proxy. proxy rep -> Response -> Except (Request -> Error) response
```

##### Instances
``` purescript
DecodeResponse None None
DecodeResponse String String
(RowToList responses responseList, DecodeResponseVariant result responseList) => DecodeResponse (Record responses) (Variant result)
```

#### `DecodeResponseVariant`

``` purescript
class DecodeResponseVariant (response :: # Type) (responseList :: RowList) | responseList -> response where
  decodeResponseVariant :: forall proxy. proxy responseList -> Response -> Except (Request -> Error) (Variant response)
```

##### Instances
``` purescript
DecodeResponseVariant () Nil
(IsSymbol status, ResponseStatus status, Cons status decoded variant' variant, DecodeResponseVariant variant' responseList, Union variant' a variant, DecodeMedia rep decoded) => DecodeResponseVariant variant (Cons status rep responseList)
```

#### `toStatus`

``` purescript
toStatus :: forall response responseList. RowToList response responseList => ResponseVariantToStatus response responseList => Variant response -> Status
```

#### `ResponseVariantToStatus`

``` purescript
class ResponseVariantToStatus (response :: # Type) (responseList :: RowList) | responseList -> response where
  responseVariantToStatus :: forall proxy. proxy responseList -> Variant response -> Status
```

##### Instances
``` purescript
ResponseVariantToStatus response Nil
(IsSymbol status, ResponseStatus status, Cons status body response' response, ResponseVariantToStatus response responseList) => ResponseVariantToStatus response (Cons status body responseList)
```


