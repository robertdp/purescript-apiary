module Apiary.Media
  ( class DecodeMedia
  , class EncodeMedia
  , class MediaType
  , JSON
  , None
  , decodeMedia
  , encodeMedia
  , mediaType
  , none
  ) where

import Prelude
import Control.Alt ((<|>))
import Data.Maybe (Maybe(..))
import Data.MediaType (MediaType)
import Data.MediaType.Common (applicationJSON, textPlain)
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Data.Variant (Variant)
import Data.Variant as Variant
import Foreign (F, Foreign, ForeignError(..), fail)
import Foreign.Index (readProp)
import Foreign.Object as Object
import Global.Unsafe (unsafeStringify)
import Partial.Unsafe (unsafeCrashWith)
import Prim.Row as Row
import Prim.RowList (class RowToList, kind RowList, Cons, Nil)
import Simple.JSON (class ReadForeign, class WriteForeign, parseJSON, readImpl, readJSON', writeImpl, writeJSON)
import Type.Data.RowList (RLProxy(..))
import Type.Proxy (Proxy)
import Unsafe.Coerce (unsafeCoerce)

class MediaType rep where
  mediaType :: Proxy rep -> Maybe MediaType

class EncodeMedia rep a | rep -> a where
  encodeMedia :: Proxy rep -> a -> String

class DecodeMedia rep a | rep -> a where
  decodeMedia :: Proxy rep -> String -> F a

foreign import data None :: Type

none :: None
none = unsafeCoerce unit

instance showNone :: Show None where
  show _ = "none"

instance semigroupNone :: Semigroup None where
  append _ _ = none

instance monoidNone :: Monoid None where
  mempty = none

instance mediaTypeNone :: MediaType None where
  mediaType _ = Nothing

instance encodeMediaNone :: EncodeMedia None None where
  encodeMedia _ _ = ""

instance decodeMediaNone :: DecodeMedia None None where
  decodeMedia _ _ = pure none

instance mediaTypeString :: MediaType String where
  mediaType _ = Just textPlain

instance encodeMediaString :: EncodeMedia String String where
  encodeMedia _ a = a

instance decodeMediaString :: DecodeMedia String String where
  decodeMedia _ a = pure a

data JSON a
  = JSON

instance mediaTypeJSON :: MediaType (JSON a) where
  mediaType _ = Just applicationJSON

instance encodeMediaJSONVariant ::
  ( RowToList row rl
  , WriteForeignVariant rl row
  ) =>
  EncodeMedia (JSON (Variant row)) (Variant row) where
  encodeMedia _ = unsafeStringify <<< writeForeignVariant (RLProxy :: _ rl)
else instance encodeMediaJSON ::
  WriteForeign a =>
  EncodeMedia (JSON a) a where
  encodeMedia _ = writeJSON

instance decodeMediaJSONVariant ::
  ( RowToList r rl
  , ReadForeignVariant rl r
  ) =>
  DecodeMedia (JSON (Variant r)) (Variant r) where
  decodeMedia _ = parseJSON >=> readForeignVariant (RLProxy :: _ rl)
else instance decodeMediaJSON ::
  ReadForeign a =>
  DecodeMedia (JSON a) a where
  decodeMedia _ = readJSON'

class ReadForeignVariant (rl :: RowList) (r :: #Type) | rl -> r where
  readForeignVariant :: RLProxy rl -> Foreign -> F (Variant r)

instance readForeignVariantNil :: ReadForeignVariant Nil r where
  readForeignVariant _ _ = fail $ ForeignError "Unable to match any variant member."

instance readForeignVariantCons ::
  ( IsSymbol name
  , ReadForeign value
  , Row.Cons name value r' r
  , ReadForeignVariant rl r
  ) =>
  ReadForeignVariant (Cons name value rl) r where
  readForeignVariant _ o =
    (Variant.inj proxy <$> (readProp name o >>= readImpl))
      <|> readForeignVariant (RLProxy :: _ rl) o
    where
    proxy = SProxy :: _ name

    name = reflectSymbol proxy

class WriteForeignVariant (rl :: RowList) (row :: #Type) | rl -> row where
  writeForeignVariant :: forall proxy. proxy rl -> Variant row -> Foreign

instance writeForeignVariantNil :: WriteForeignVariant Nil () where
  writeForeignVariant _ _ = unsafeCrashWith "Variant was not able to be written. This should be unreachable"

instance writeForeignVariantCons ::
  ( IsSymbol name
  , WriteForeign value
  , Row.Cons name value r row
  , WriteForeignVariant rl r
  ) =>
  WriteForeignVariant (Cons name value rl) row where
  writeForeignVariant _ variant =
    Variant.on
      name
      writeVariant
      (writeForeignVariant (RLProxy :: _ rl))
      variant
    where
    name = SProxy :: _ name

    writeVariant value = writeImpl $ Object.insert (reflectSymbol name) (writeImpl value) Object.empty
