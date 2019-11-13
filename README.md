<pre>
    _          _
   / \   _ __ (_) __ _ _ __ _   _                .' '.            __
  / _ \ | '_ \| |/ _` | '__| | | |      .        .   .           (__\_
 / ___ \| |_) | | (_| | |  | |_| |       .         .         . -{{_(|8)
/_/   \_\ .__/|_|\__,_|_|   \__, |         ' .  . ' ' .  . '     (__/
        |_|                 |___/
</pre>

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

type CreateNewUser
  = POST "/users"
    { body :: JSON { name :: String, email :: String }
    , response ::
      { ok :: JSON User
      , badRequest :: JSON { errors :: Array { field :: String, message :: String } }
      }
    }

listUsers params =
  Apiary.makeRequest (Route :: ListUsers) identity params unit

createNewUser body =
  Apiary.makeRequest (Route :: CreateNewUser) identity {} body
```

This will give us inferred types equivalent to

```purescript
listUsers ::
  { sortBy :: Maybe UserSort, sortDir :: Maybe SortDir } ->
  Aff (Either Apiary.Error (Variant ( ok :: Array User )))

createNewUser ::
  { name :: String, email :: String } ->
  Aff
    (Either Apiary.Error
      (Variant
        ( ok :: User
        , badRequest :: { errors :: Array { field :: String, message :: String } }
        )
      )
    )
```

Here is an example with URL modification and JWT-based authorisation inspired by some production code:

```purescript
makeSecureRequest ::
  forall m r route params body rep response.
  MonadAff m =>
  MonadAsk { baseUrl :: String, token :: String | r } m =>
  Apiary.BuildRequest route params body rep =>
  Apiary.DecodeResponse rep response =>
  route ->
  params ->
  body ->
  m (Either Apiary.Error response)
makeSecureRequest route params body = do
  env <- ask
  liftAff $ Apiary.makeRequest route (addBaseUrl env.baseUrl <<< addToken env.token) params body
  where
  addToken token request@{ headers } =
    request { headers = Object.insert "Authorization" ("Bearer " <> token) headers }
  addBaseUrl baseUrl request@{ url: URL url } =
    request { url = URL (baseUrl <> url) }

listUsers ::
  { sortBy :: Maybe UserSort, sortDir :: Maybe SortDir } ->
  AppM (Either Apiary.Error (Variant ( ok :: Array User )))
listUsers params =
  makeSecureRequest (Route :: ListUsers) identity params unit
```
