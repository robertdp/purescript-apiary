module Apiary.Body where

import Prelude

import Foreign (F)
import Simple.JSON (class ReadForeign, class WriteForeign, readJSON', writeJSON)
import Type.Proxy (Proxy)

class DecodeBody rep a | rep -> a where
  decodeBody :: Proxy rep -> String -> F a

instance decodeBodyUnit :: DecodeBody Unit Unit where
  decodeBody _ _ = pure unit

instance decodeBodyString :: DecodeBody String String where
  decodeBody _ = pure

newtype JSON a = JSON a

instance decodeBodyJSON :: (ReadForeign a) => DecodeBody (JSON a) a where
  decodeBody _ = readJSON'

class EncodeBody rep a | rep -> a where
  encodeBody :: Proxy rep -> a -> String

instance encodeBodyString :: EncodeBody String String where
  encodeBody _ = identity

instance encodeBodyJSON :: (WriteForeign a) => EncodeBody (JSON a) a where
  encodeBody _ = writeJSON
