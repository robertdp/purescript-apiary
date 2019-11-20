module Apiary.Url where

import Prelude
import Foreign (F)
import Global.Unsafe (unsafeDecodeURIComponent, unsafeEncodeURIComponent)
import Simple.JSON (readJSON', writeJSON)

class UrlParam a where
  encodeUrlParam :: a -> String
  decodeUrlParam :: String -> F a

instance urlParamString :: UrlParam String where
  encodeUrlParam = unsafeEncodeURIComponent
  decodeUrlParam = unsafeDecodeURIComponent >>> pure

instance urlParamInt :: UrlParam Int where
  encodeUrlParam = encodeUrlParam <<< writeJSON
  decodeUrlParam = decodeUrlParam >=> readJSON'

instance urlParamNumber :: UrlParam Number where
  encodeUrlParam = encodeUrlParam <<< writeJSON
  decodeUrlParam = decodeUrlParam >=> readJSON'
