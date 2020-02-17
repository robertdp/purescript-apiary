module Apiary.Route where

import Apiary.Media (None)
import Prim.Row (class Nub, class Union)

data Route (method :: Symbol) (path :: Symbol) spec
  = Route

type SpecDefaults
  = ( path :: {}
    , query :: {}
    , body :: None
    , response :: None
    )

class PrepareSpec spec prepared | spec -> prepared

instance prepareSpec ::
  ( Union spec SpecDefaults specWithDefaults
  , Nub specWithDefaults prepared
  ) =>
  PrepareSpec (Record spec) (Record prepared)

type GET
  = Route "GET"

type PATCH
  = Route "PATCH"

type POST
  = Route "POST"

type PUT
  = Route "PUT"

type DELETE
  = Route "DELETE"
