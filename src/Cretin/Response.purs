module Cretin.Response where

import Prelude

import Control.Alt ((<|>))
import Cretin.Body (class DecodeBody, decodeBody)
import Cretin.Types (Response)
import Data.Symbol (class IsSymbol)
import Data.Variant (SProxy(..), Variant, expand, inj)
import Foreign (F, ForeignError(..), fail)
import Prim.Row (class Cons, class Union)
import Prim.RowList (class RowToList, kind RowList, Cons, Nil)
import Type.Data.RowList (RLProxy(..))
import Type.Proxy (Proxy(..))
import Type.RowList (class ListToRow)

class DecodeResponse rep response | rep -> response where
  decodeResponse :: Proxy rep -> Response -> F response

instance decodeResponseUnit :: DecodeResponse Unit Unit where
  decodeResponse _ _ = pure unit

instance decodeResponseString :: DecodeResponse String String where
  decodeResponse _ response = pure response.body

instance decodeResponseVariant_ ::
  ( RowToList responses responseList
  , ListToRow resultList result
  , DecodeResponseVariant result responseList
  ) =>
  DecodeResponse { | responses } (Variant result) where
  decodeResponse _ = decodeResponseVariant (RLProxy :: _ responseList)

class DecodeResponseStatus status where
  decodeResponseStatus :: forall rep a. DecodeBody rep a => SProxy status -> Proxy rep -> Response -> F a

instance decodeResponseOK :: DecodeResponseStatus "ok" where
  decodeResponseStatus _ = decodeResponseWithStatus 200

instance decodeResponseBadRequest :: DecodeResponseStatus "badRequest" where
  decodeResponseStatus _ = decodeResponseWithStatus 304

instance decodeResponseNotFound :: DecodeResponseStatus "notFound" where
  decodeResponseStatus _ = decodeResponseWithStatus 404

decodeResponseWithStatus :: forall rep a. DecodeBody rep a => Int -> Proxy rep -> Response -> F a
decodeResponseWithStatus status rep response
  | response.status == status = decodeBody rep response.body
  | otherwise = fail $ ForeignError $ "Failed to match status code " <> show response.status

class DecodeRowList (rep :: RowList) (decoded :: RowList) | rep -> decoded

instance decodeRowListNil :: DecodeRowList Nil Nil

instance decodeRowListCons ::
  ( DecodeBody rep decoded
  , DecodeRowList repTail decodedTail
  ) =>
  DecodeRowList (Cons name rep repTail) (Cons name decoded decodedTail)

class DecodeResponseVariant (response :: #Type) (responseList :: RowList) | responseList -> response where
  decodeResponseVariant :: RLProxy responseList -> Response -> F (Variant response)

instance decodeResponseVariantNil :: DecodeResponseVariant () Nil where
  decodeResponseVariant _ response = fail $ ForeignError $ "Failed to match response with status code " <> show response.status

instance decodeResponseVariantCons ::
  ( IsSymbol status
  , DecodeResponseStatus status
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

    decodeStatus = decodeResponseStatus status (Proxy :: _ rep) response

    decodeRest = decodeResponseVariant (RLProxy :: _ responseList) response
