# Apiary

It's hard to be a busy little bee when working with unspec'ed APIs. This library is designed for the creation of type-level specs that can be queried against automatically!

### Example

```purescript
type ListUsers
  = GET "/users"
    { query ::
      { sortBy :: UserSort
      , sortDir :: SortDir
      }
    , response ::
      { ok :: JSON (Array { id :: Int, name :: String, email :: String })
      }
    }

type CreateNewUser
  = POST "/users"
    { body ::
      JSON { name :: String, email :: String }
    , response ::
      { ok :: JSON (Array { id :: Int, name :: String, email :: String })
      , badRequest :: JSON { errors :: Array { field :: String, message :: String } }
      }

listUsers ::
  { sortBy :: Maybe UserSort, sortDir :: Maybe SortDir } ->
  Unit ->
  Aff (Variant ( ok :: Array { id :: Int, name :: String, email :: String } ))
listUsers = Apiary.makeRequest (Route :: ListUsers) identity

createNewUser ::
  { name :: String, email :: String } ->
  Aff
    (Variant
      ( ok :: Array { id :: Int, name :: String, email :: String }
      , badRequest :: { errors :: Array { field :: String, message :: String } }
      )
    )
createNewUser = Apiary.makeRequest (Route :: CreateNewUser) identity {}
```
