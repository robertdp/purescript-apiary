module Apiary.Request where

import Prelude

import Apiary.Params (class WriteParams)
import Apiary.Response (class DecodeResponse)
import Apiary.Route (Route)
import Data.Symbol (class IsSymbol)
import Effect.Aff (Aff)
import Milkis (Headers)
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
  , WriteParams params
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
