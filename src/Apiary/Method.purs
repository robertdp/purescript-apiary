module Apiary.Method
  ( class RequestMethod
  , toMethod
  ) where

import Prelude
import Data.Symbol (class IsSymbol, SProxy, reflectSymbol)
import Milkis (Method)
import Partial.Unsafe (unsafeCrashWith)
import Prim.TypeError (class Fail, Beside, Text)
import Unsafe.Coerce (unsafeCoerce)

class RequestMethod (method :: Symbol) where
  toMethod :: SProxy method -> Method

instance requestMethodGET :: RequestMethod "GET" where
  toMethod = coerceMethod
else instance requestMethodPOST :: RequestMethod "POST" where
  toMethod = coerceMethod
else instance requestMethodPUT :: RequestMethod "PUT" where
  toMethod = coerceMethod
else instance requestMethodHEAD :: RequestMethod "PATCH" where
  toMethod = coerceMethod
else instance requestMethodDELETE :: RequestMethod "DELETE" where
  toMethod = coerceMethod
else instance requestMethodFail ::
  Fail (Beside (Text "Unsupported request method: ") (Text method)) =>
  RequestMethod method where
  toMethod _ = unsafeCrashWith "impossible"

coerceMethod :: forall method. IsSymbol method => SProxy method -> Method
coerceMethod = unsafeCoerce <<< reflectSymbol
