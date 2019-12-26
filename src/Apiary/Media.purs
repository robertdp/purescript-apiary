module Apiary.Media where

import Prelude
import Apiary.Types (None(..))
import Data.Maybe (Maybe(..))
import Data.MediaType (MediaType)
import Data.MediaType.Common (applicationJSON, textPlain)
import Foreign (F)
import Simple.JSON (class ReadForeign, class WriteForeign, readJSON', writeJSON)
import Type.Proxy (Proxy)

class MediaType rep where
  mediaType :: Proxy rep -> Maybe MediaType

class EncodeMedia rep a | rep -> a where
  encodeMedia :: Proxy rep -> a -> String

class DecodeMedia rep a | rep -> a where
  decodeMedia :: Proxy rep -> String -> F a

instance mediaTypeNone :: MediaType None where
  mediaType _ = Nothing

instance encodeMediaNone :: EncodeMedia None None where
  encodeMedia _ _ = ""

instance decodeMediaNone :: DecodeMedia None None where
  decodeMedia _ _ = pure None

instance mediaTypeString :: MediaType String where
  mediaType _ = Just textPlain

instance encodeMediaString :: EncodeMedia String String where
  encodeMedia _ a = a

instance decodeMediaString :: DecodeMedia String String where
  decodeMedia _ a = pure a

newtype JSON a
  = JSON a

instance mediaTypeJSON :: MediaType (JSON a) where
  mediaType _ = Just applicationJSON

instance encodeMediaJSON :: (WriteForeign a) => EncodeMedia (JSON a) a where
  encodeMedia _ = writeJSON

instance decodeMediaJSON :: (ReadForeign a) => DecodeMedia (JSON a) a where
  decodeMedia _ = readJSON'
