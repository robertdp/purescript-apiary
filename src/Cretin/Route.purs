module Apiary.Route where

data Route (method :: Symbol) (path :: Symbol) spec
  = Route

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
