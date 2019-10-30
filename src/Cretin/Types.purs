module Apiary.Types where

import Prelude

import Data.Maybe (Maybe)
import Milkis (Headers, Method, URL(..), getMethod)

type Request
  = { method :: Method
    , url :: URL
    , headers :: Headers
    , body :: Maybe String
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
