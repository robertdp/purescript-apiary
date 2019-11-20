## Module Apiary.Route

#### `Route`

``` purescript
data Route (method :: Symbol) (path :: Symbol) spec
  = Route
```

#### `SpecDefaults`

``` purescript
type SpecDefaults = (body :: Unit, params :: Record (), query :: Record (), response :: Unit)
```

#### `PrepareSpec`

``` purescript
class PrepareSpec spec prepared | spec -> prepared
```

##### Instances
``` purescript
(Union spec SpecDefaults specWithDefaults, Nub specWithDefaults prepared) => PrepareSpec (Record spec) (Record prepared)
```

#### `GET`

``` purescript
type GET = Route "GET"
```

#### `PATCH`

``` purescript
type PATCH = Route "PATCH"
```

#### `POST`

``` purescript
type POST = Route "POST"
```

#### `PUT`

``` purescript
type PUT = Route "PUT"
```

#### `DELETE`

``` purescript
type DELETE = Route "DELETE"
```


