module Apiary.Types where

import Prelude
import Data.Symbol (SProxy(..))
import Effect.Exception as Exception
import Foreign (MultipleErrors)
import Milkis (Headers, Method, URL(..), getMethod)
import Record as Record
import Unsafe.Coerce (unsafeCoerce)

type Request
  = { method :: Method
    , url :: URL
    , headers :: Headers
    , body :: String
    }

emptyRequest :: Request
emptyRequest =
  { method: getMethod
  , url: URL mempty
  , headers: mempty
  , body: mempty
  }

type Response
  = { status :: Int
    , headers :: Headers
    , body :: String
    }

data Error
  = RuntimeError Exception.Error
  | DecodeError Request Response MultipleErrors
  | UnexpectedResponse Request Response

showRequest :: Request -> String
showRequest = show <<< Record.modify (SProxy :: _ "method") (unsafeCoerce :: Method -> String)

instance showError :: Show Error where
  show (RuntimeError err) = "(RuntimeError " <> show err <> ")"
  show (DecodeError req res err) = "(DecodeError " <> showRequest req <> " " <> show res <> " " <> show err <> ")"
  show (UnexpectedResponse req res) = "(UnexpectedResponse " <> showRequest req <> " " <> show res <> ")"

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
