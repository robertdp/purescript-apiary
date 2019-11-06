module Apiary.Response where

import Prelude
import Apiary.Body (class DecodeBody, decodeBody)
import Apiary.Status (class ResponseStatus)
import Apiary.Status as Status
import Apiary.Types (Apiary, Error(..), Response)
import Control.Alt ((<|>))
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except (withExcept)
import Data.Symbol (class IsSymbol)
import Data.Variant (SProxy(..), Variant, expand, inj)
import Prim.Row (class Cons, class Union)
import Prim.RowList (class RowToList, kind RowList, Cons, Nil)
import Type.Data.RowList (RLProxy(..))
import Type.Proxy (Proxy(..))

class DecodeResponse rep response | rep -> response where
  decodeResponse :: Proxy rep -> Response -> Apiary response

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
  decodeResponseVariant :: RLProxy responseList -> Response -> Apiary (Variant response)

instance decodeResponseVariantNil :: DecodeResponseVariant () Nil where
  -- | This will never be reached if the data originates from PureScript.
  decodeResponseVariant _ = throwError <<< UnexpectedResponse

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
      | response.status == statusCode = withExcept (DecodeError response) $ decodeBody (Proxy :: _ rep) response.body
      | otherwise = throwError $ UnexpectedResponse response

    decodeRest = decodeResponseVariant (RLProxy :: _ responseList) response
