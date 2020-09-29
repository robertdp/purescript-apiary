module Apiary.Types where

import Prelude
import Affjax as Affajx
import Affjax as Affjax
import Affjax.RequestHeader (RequestHeader)
import Affjax.ResponseHeader (ResponseHeader)
import Affjax.StatusCode (StatusCode)
import Data.HTTP.Method (Method(..))
import Foreign (MultipleErrors)
import Unsafe.Coerce (unsafeCoerce)

type URL
  = String

type Request
  = { method :: Method
    , url :: URL
    , headers :: Array RequestHeader
    , body :: String
    }

emptyRequest :: Request
emptyRequest =
  { method: GET
  , url: ""
  , headers: []
  , body: ""
  }

type Response
  = { status :: StatusCode
    , headers :: Array ResponseHeader
    , body :: String
    }

data Error
  = RuntimeError Affjax.Error
  | DecodeError Request Response MultipleErrors
  | UnexpectedResponse Request Response

instance showError :: Show Error where
  show (RuntimeError err) = "(RuntimeError {- " <> Affajx.printError err <> " -} )"
  show (DecodeError req res err) = "(DecodeError " <> show req <> " " <> show res <> " " <> show err <> ")"
  show (UnexpectedResponse req res) = "(UnexpectedResponse " <> show req <> " " <> show res <> ")"

instance semigroupError :: Semigroup Error where
  append err@(RuntimeError _) _ = err
  append _ err@(RuntimeError _) = err
  append err@(DecodeError _ _ _) _ = err
  append _ err@(DecodeError _ _ _) = err
  append err@(UnexpectedResponse _ _) _ = err

foreign import data None :: Type

none :: None
none = unsafeCoerce unit

instance showNone :: Show None where
  show _ = "none"

instance semigroupNone :: Semigroup None where
  append _ _ = none

instance monoidNone :: Monoid None where
  mempty = none
