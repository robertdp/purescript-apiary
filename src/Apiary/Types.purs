module Apiary.Types where

import Control.Monad.Except (Except)
import Data.Monoid (mempty)
import Data.Semigroup (class Semigroup)
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
  | DecodeError Response MultipleErrors
  | UnexpectedResponse Response

instance semigroupError :: Semigroup Error where
  append err@(RuntimeError _) _ = err
  append _ err@(RuntimeError _) = err
  append err@(DecodeError _ _) _ = err
  append _ err@(DecodeError _ _) = err
  append err@(UnexpectedResponse _) _ = err

type Apiary
  = Except Error
