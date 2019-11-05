# Apiary

It's hard to be a busy little bee when working with unspec'ed APIs. This library is designed for the creation of type-level specs that can be queried against automatically!

### Example

```purescript
type User
  = { id :: Int
    , name :: String
    , email :: String
    }

type ListUsers
  = GET "/users"
    { query ::
      { sortBy :: UserSort
      , sortDir :: SortDir
      }
    , response ::
      { ok :: JSON (Array User)
      }
    }

listUsers ::
  { sortBy :: Maybe UserSort
  , sortDir :: Maybe SortDir
  } ->
  Aff (Variant ( ok :: Array User ))
listUsers params = Apiary.makeRequest (Route :: ListUsers) identity params unit

type CreateNewUser
  = POST "/users"
    { body ::
      JSON
        { name :: String
        , email :: String
        }
    , response ::
      { ok :: JSON User
      , badRequest ::
        JSON
          { errors ::
            Array
              { field :: String
              , message :: String
              }
          }
      }
    }

createNewUser ::
  { name :: String
  , email :: String
  } ->
  Aff
    (Variant
      ( ok :: User
      , badRequest ::
        { errors ::
          Array
            { field :: String
            , message :: String
            }
        }
      )
    )
createNewUser = Apiary.makeRequest (Route :: CreateNewUser) identity {}
```
