module Apiary.Body where

import Foreign (F)
import Type.Proxy (Proxy)

class DecodeBody rep a | rep -> a where
  decodeBody :: Proxy rep -> String -> F a

class EncodeBody rep a | rep -> a where
  encodeBody :: Proxy rep -> a -> String

