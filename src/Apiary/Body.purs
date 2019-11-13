module Apiary.Body where

import Prelude
import Data.Maybe (Maybe(..))
import Data.MediaType (MediaType)
import Data.MediaType.Common (applicationJSON)
import Foreign (F)
import Simple.JSON (class ReadForeign, class WriteForeign, readJSON', writeJSON)
import Type.Proxy (Proxy)

class MediaCodec rep a | rep -> a where
  mediaType :: Proxy rep -> Maybe MediaType
  encodeMedia :: Proxy rep -> a -> String
  decodeMedia :: Proxy rep -> String -> F a

newtype JSON a
  = JSON a

instance mediaCodecUnit :: MediaCodec Unit Unit where
  mediaType _ = Nothing
  encodeMedia _ _ = ""
  decodeMedia _ _ = pure unit

instance mediaCodecString :: MediaCodec String String where
  mediaType _ = Nothing
  encodeMedia _ a = a
  decodeMedia _ a = pure a

instance mediaCodecJSON :: (WriteForeign a, ReadForeign a) => MediaCodec (JSON a) a where
  mediaType _ = Just applicationJSON
  encodeMedia _ = writeJSON
  decodeMedia _ = readJSON'
