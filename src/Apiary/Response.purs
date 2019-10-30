module Apiary.Response where

import Prelude

import Apiary.Body (class DecodeBody, decodeBody)
import Apiary.Types (Response)
import Control.Alt ((<|>))
import Data.Symbol (class IsSymbol)
import Data.Variant (SProxy(..), Variant, expand, inj)
import Effect.Exception.Unsafe (unsafeThrow)
import Foreign (F, ForeignError(..), fail)
import Prim.Row (class Cons, class Union)
import Prim.RowList (class RowToList, kind RowList, Cons, Nil)
import Prim.TypeError (class Fail, Beside, Text)
import Type.Data.RowList (RLProxy(..))
import Type.Proxy (Proxy(..))

class DecodeResponse rep response | rep -> response where
  decodeResponse :: Proxy rep -> Response -> F response

instance decodeResponseUnit :: DecodeResponse Unit Unit where
  decodeResponse _ _ = pure unit

instance decodeResponseString :: DecodeResponse String String where
  decodeResponse _ response = pure response.body

instance decodeResponseVariant_ ::
  ( RowToList responses responseList
  , DecodeResponseVariant result responseList
  ) =>
  DecodeResponse { | responses } (Variant result) where
  decodeResponse _ = decodeResponseVariant (RLProxy :: _ responseList)

class DecodeResponseStatus status where
  decodeResponseStatus :: forall rep a. DecodeBody rep a => SProxy status -> Proxy rep -> Response -> F a

instance decodeResponseOk :: DecodeResponseStatus "ok" where
  decodeResponseStatus _ = decodeResponseWithStatus 200
else instance decodeResponseCreated :: DecodeResponseStatus "created" where
  decodeResponseStatus _ = decodeResponseWithStatus 201
else instance decodeResponseNoContent :: DecodeResponseStatus "noContent" where
  decodeResponseStatus _ = decodeResponseWithStatus 204
else instance decodeResponseNotModified :: DecodeResponseStatus "notModified" where
  decodeResponseStatus _ = decodeResponseWithStatus 304
else instance decodeResponseBadRequest :: DecodeResponseStatus "badRequest" where
  decodeResponseStatus _ = decodeResponseWithStatus 400
else instance decodeResponseUnauthorized :: DecodeResponseStatus "unauthorized" where
  decodeResponseStatus _ = decodeResponseWithStatus 401
else instance decodeResponseForbidden :: DecodeResponseStatus "forbidden" where
  decodeResponseStatus _ = decodeResponseWithStatus 403
else instance decodeResponseNotFound :: DecodeResponseStatus "notFound" where
  decodeResponseStatus _ = decodeResponseWithStatus 404
else instance decodeResponseConflict :: DecodeResponseStatus "conflict" where
  decodeResponseStatus _ = decodeResponseWithStatus 409
else instance decodeResponseInternalServerError :: DecodeResponseStatus "internalServerError" where
  decodeResponseStatus _ = decodeResponseWithStatus 500
else instance decodeResponseMaintenanceInProgress :: DecodeResponseStatus "maintenanceInProgress" where
  decodeResponseStatus _ = decodeResponseWithStatus 520
else instance decodeResponseFailure :: (Fail (Beside (Text "Unknown response status: ") (Text unknown))) => DecodeResponseStatus unknown where
  decodeResponseStatus _ _ _ = unsafeThrow "this is impossible"

decodeResponseWithStatus :: forall rep a. DecodeBody rep a => Int -> Proxy rep -> Response -> F a
decodeResponseWithStatus status rep response
  | response.status == status = decodeBody rep response.body
  | otherwise = fail $ ForeignError $ "Failed to match status code " <> show response.status

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


test = decodeResponse (Proxy :: _ { thisIsNotAStatus :: String })
