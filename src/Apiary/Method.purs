module Apiary.Method
  ( module Data.HTTP.Method
  , class RequestMethod
  , toMethod
  ) where

import Data.HTTP.Method (Method(..))
import Data.HTTP.Method (Method(..)) as Data.HTTP.Method
import Data.Symbol (SProxy)
import Partial.Unsafe (unsafeCrashWith)
import Prim.TypeError (class Fail, Beside, Text)

class RequestMethod (method :: Symbol) where
  toMethod :: SProxy method -> Method

instance requestMethodGET :: RequestMethod "GET" where
  toMethod _ = GET
else instance requestMethodPOST :: RequestMethod "POST" where
  toMethod _ = POST
else instance requestMethodPUT :: RequestMethod "PUT" where
  toMethod _ = PUT
else instance requestMethodHEAD :: RequestMethod "PATCH" where
  toMethod _ = PATCH
else instance requestMethodDELETE :: RequestMethod "DELETE" where
  toMethod _ = DELETE
else instance requestMethodFail ::
  Fail (Beside (Text "Unsupported request method: ") (Text method)) =>
  RequestMethod method where
  toMethod _ = unsafeCrashWith "impossible"
