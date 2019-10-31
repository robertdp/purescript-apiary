module Apiary.Request where

import Prelude
import Apiary.Params (class WriteParams)
import Apiary.Response (class DecodeResponse)
import Apiary.Route (Route)
import Data.Symbol (class IsSymbol)
import Effect.Aff (Aff)
import Milkis (Headers)
import Type.Equality (class TypeEquals)
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
