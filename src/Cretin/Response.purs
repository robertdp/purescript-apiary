module Cretin.Response where

import Prelude
import Control.Alt ((<|>))
import Cretin.Types (Response)
import Data.Identity (Identity(..))
import Data.Symbol (class IsSymbol)
import Data.Variant (SProxy(..), Variant, expand, inj)
import Effect.Exception.Unsafe (unsafeThrow)
import Foreign (F, ForeignError(..), fail)
import Prim.Row (class Cons, class Union)
import Prim.RowList (class RowToList, kind RowList, Cons, Nil)
import Simple.JSON (class ReadForeign)
import Type.Data.RowList (RLProxy(..))
import Type.Equality (class TypeEquals)
import Type.Proxy (Proxy(..))
import Type.RowList (class ListToRow)
import Unsafe.Coerce (unsafeCoerce)

class DecodeBody rep a | rep -> a where
  decodeBody :: Proxy rep -> String -> F a

class DecodeResponse rep response | rep -> response where
  decodeResponse :: Proxy rep -> String -> F response

instance decodeResponseUnit :: DecodeResponse Unit Unit where
  decodeResponse _ _ = pure unit

instance decodeResponseString :: DecodeResponse String String where
  decodeResponse _ = pure

instance decodeResponseVariant_ ::
  ( RowToList responses responseList
  , DecodeRowList responseList resultList
  , ListToRow resultList result
  ) =>
  DecodeResponse { | responses } (Variant result) where
  decodeResponse _ _ = unsafeCoerce unit

class DecodeResponseStatus status rep response | rep -> response where
  decodeResponseStatus :: SProxy status -> Proxy rep -> Response -> F response

instance decodeResponseOK ::
  (DecodeBody rep response) =>
  DecodeResponseStatus "ok" rep response where
  decodeResponseStatus _ _ response
    | response.status == 200 = decodeBody (Proxy :: _ rep) response.body
    | otherwise = fail $ ForeignError $ "Failed to match status code " <> show response.status

instance decodeResponseBadRequest ::
  (DecodeBody rep response) =>
  DecodeResponseStatus "badRequest" rep response where
  decodeResponseStatus _ _ response
    | response.status == 304 = decodeBody (Proxy :: _ rep) response.body
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
  , DecodeResponseStatus status rep decoded
  , Cons status decoded variant' variant
  , DecodeResponseVariant variant' responseList
  , Union variant' a variant
  ) =>
  DecodeResponseVariant variant (Cons status rep responseList) where
  decodeResponseVariant _ response =
    (inj status <$> decodeStatus)
      <|> (expand <$> decodeRest)
    where
    status = SProxy :: _ status

    decodeStatus = decodeResponseStatus status (Proxy :: _ rep) response

    decodeRest = decodeResponseVariant (RLProxy :: _ responseList) response
