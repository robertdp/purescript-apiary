module Apiary.Response where

import Prelude
import Apiary.Body (class DecodeBody, decodeBody)
import Apiary.Status (class ResponseStatus)
import Apiary.Status as Status
import Apiary.Types (Response)
import Control.Alt ((<|>))
import Data.Symbol (class IsSymbol)
import Data.Variant (SProxy(..), Variant, expand, inj)
import Foreign (F, ForeignError(..), fail)
import Prim.Row (class Cons, class Union)
import Prim.RowList (class RowToList, kind RowList, Cons, Nil)
import Type.Data.RowList (RLProxy(..))
import Type.Proxy (Proxy(..))

class DecodeResponse rep response | rep -> response where
  decodeResponse :: Proxy rep -> Response -> F response

instance decodeResponseUnit :: DecodeResponse Unit Unit where
  decodeResponse _ _ = pure unit

instance decodeResponseString :: DecodeResponse String String where
  decodeResponse _ response = pure response.body

instance decodeResponseRecord ::
  ( RowToList responses responseList
  , DecodeResponseVariant result responseList
  ) =>
  DecodeResponse { | responses } (Variant result) where
  decodeResponse _ = decodeResponseVariant (RLProxy :: _ responseList)

class DecodeResponseVariant (response :: #Type) (responseList :: RowList) | responseList -> response where
  decodeResponseVariant :: RLProxy responseList -> Response -> F (Variant response)

instance decodeResponseVariantNil :: DecodeResponseVariant () Nil where
  -- | This will never be reached if the data originates from PureScript.
  decodeResponseVariant _ response = fail $ ForeignError $ "Failed to match any variant with status code " <> show response.status

instance decodeResponseVariantCons ::
  ( IsSymbol status
  , ResponseStatus status
  , Cons status decoded variant' variant
  , DecodeResponseVariant variant' responseList
  , Union variant' a variant
  , DecodeBody rep decoded
  ) =>
  DecodeResponseVariant variant (Cons status rep responseList) where
  decodeResponseVariant _ response =
    (inj status <$> decodeStatus)
      <|> (expand <$> decodeRest)
    where
    status = SProxy :: _ status

    statusCode = Status.statusCode $ Status.toStatus status

    decodeStatus
      | response.status == statusCode = decodeBody (Proxy :: _ rep) response.body
      | otherwise = fail $ ForeignError $ "Failed to match status code " <> show response.status

    decodeRest = decodeResponseVariant (RLProxy :: _ responseList) response
