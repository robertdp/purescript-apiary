module Cretin.Request where

import Prelude
import Cretin.Params (class EncodeParams)
import Cretin.Response (class DecodeResponse)
import Cretin.Route (Route)
import Data.Symbol (class IsSymbol)
import Effect.Aff (Aff)
import Milkis (Method, URL, Headers)
import Simple.JSON (class WriteForeign, writeJSON)
import Type.Equality (class TypeEquals)
import Type.Proxy (Proxy)
import Unsafe.Coerce (unsafeCoerce)

class Requestable route params body response | route -> params body response where
  makeRequest :: route -> (Headers -> Headers) -> params -> body -> Aff response

instance requestableRoute ::
  ( IsSymbol path
  , TypeEquals
      spec
      { params :: params
      , response :: response'
      }
  , EncodeParams params
  , DecodeResponse response' response
  ) =>
  Requestable (Route "GET" path spec) params body response where
  makeRequest = unsafeCoerce unit

class EncodeBody ref body | ref -> body where
  encodeBody :: Proxy ref -> body -> String

instance encodeBodyString :: EncodeBody String String where
  encodeBody _ body = body

newtype JSON a
  = JSON a

instance encodeBodyJSON :: (WriteForeign a) => EncodeBody (JSON a) a where
  encodeBody _ a = writeJSON a
