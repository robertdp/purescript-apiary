module Apiary.Url where

import Prelude
import Foreign (F)
import Global.Unsafe (unsafeDecodeURIComponent, unsafeEncodeURIComponent)
import Simple.JSON (readJSON', writeJSON)

class EncodeParam a where
  encodeParam :: a -> String

class DecodeParam a where
  decodeParam :: String -> F a

instance encodeParamString :: EncodeParam String where
  encodeParam = unsafeEncodeURIComponent

instance decodeParamString :: DecodeParam String where
  decodeParam = pure <<< unsafeDecodeURIComponent

instance encodeParamBoolean :: EncodeParam Boolean where
  encodeParam = encodeParam <<< writeJSON

instance decodeParamBoolean :: DecodeParam Boolean where
  decodeParam = decodeParam >=> readJSON'

instance encodeParamInt :: EncodeParam Int where
  encodeParam = encodeParam <<< writeJSON

instance decodeParamInt :: DecodeParam Int where
  decodeParam = decodeParam >=> readJSON'

instance encodeParamNumber :: EncodeParam Number where
  encodeParam = encodeParam <<< writeJSON

instance decodeParamNumber :: DecodeParam Number where
  decodeParam = decodeParam >=> readJSON'
