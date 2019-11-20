module Apiary.Types where

import Prelude
import Control.Monad.Except (Except)
import Data.Newtype (class Newtype)
import Effect.Exception as Exception
import Foreign (MultipleErrors)
import Milkis (Headers, Method, URL(..), getMethod)

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
  | DecodeError MultipleErrors Response
  | UnexpectedResponse Response

instance showError :: Show Error where
  show (RuntimeError err) = "(RuntimeError " <> show err <> ")"
  show (DecodeError err res) = "(DecodeError " <> show err <> " " <> show res <> ")"
  show (UnexpectedResponse res) = "(RuntimeError " <> show res <> ")"

instance semigroupError :: Semigroup Error where
  append err@(RuntimeError _) _ = err
  append _ err@(RuntimeError _) = err
  append err@(DecodeError _ _) _ = err
  append _ err@(DecodeError _ _) = err
  append err@(UnexpectedResponse _) _ = err

type Apiary
  = Except Error

newtype JSON a
  = JSON a

derive instance newtypeJSON :: Newtype (JSON a) _
