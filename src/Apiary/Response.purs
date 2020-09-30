module Apiary.Response where

import Prelude
import Apiary.Media (class DecodeMedia, decodeMedia)
import Apiary.Status (class ResponseStatus, Status)
import Apiary.Status as Status
import Apiary.Types (Error(..), None, Response, Request, none)
import Control.Alt ((<|>))
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except (Except, withExcept)
import Data.Maybe (isJust)
import Data.Symbol (class IsSymbol)
import Data.Variant (SProxy(..), Variant, expand, inj, prj)
import Partial.Unsafe (unsafeCrashWith)
import Prim.Row (class Cons, class Union)
import Prim.RowList (class RowToList, kind RowList, Cons, Nil)
import Type.Data.RowList (RLProxy(..))
import Type.Proxy (Proxy(..))

class DecodeResponse rep response | rep -> response where
  decodeResponse :: forall proxy. proxy rep -> Response -> Except (Request -> Error) response

instance decodeResponseNone :: DecodeResponse None None where
  decodeResponse _ _ = pure none

instance decodeResponseString :: DecodeResponse String String where
  decodeResponse _ response = pure response.body

instance decodeResponseRecord ::
  ( RowToList responses responseList
  , DecodeResponseVariant result responseList
  ) =>
  DecodeResponse { | responses } (Variant result) where
  decodeResponse _ = decodeResponseVariant (RLProxy :: _ responseList)

class DecodeResponseVariant (response :: # Type) (responseList :: RowList) | responseList -> response where
  decodeResponseVariant :: forall proxy. proxy responseList -> Response -> Except (Request -> Error) (Variant response)

instance decodeResponseVariantNil :: DecodeResponseVariant () Nil where
  -- | This will never be reached if the data originates from PureScript.
  decodeResponseVariant _ = throwError <<< flip UnexpectedResponse

instance decodeResponseVariantCons ::
  ( IsSymbol status
  , ResponseStatus status
  , Cons status decoded variant' variant
  , DecodeResponseVariant variant' responseList
  , Union variant' a variant
  , DecodeMedia rep decoded
  ) =>
  DecodeResponseVariant variant (Cons status rep responseList) where
  decodeResponseVariant _ response =
    (inj status <$> decodeStatus)
      <|> (expand <$> decodeRest)
    where
    status = SProxy :: _ status

    statusCode = Status.toStatusCode (Status.toStatus status)

    decodeStatus
      | response.status == statusCode = withExcept (\errs req -> DecodeError req response errs) $ decodeMedia (Proxy :: _ rep) response.body
      | otherwise = throwError $ flip UnexpectedResponse response

    decodeRest = decodeResponseVariant (RLProxy :: _ responseList) response

toStatus ::
  forall response responseList.
  RowToList response responseList =>
  ResponseVariantToStatus response responseList =>
  Variant response ->
  Status
toStatus = responseVariantToStatus (RLProxy :: _ responseList)

class ResponseVariantToStatus (response :: # Type) (responseList :: RowList) | responseList -> response where
  responseVariantToStatus :: forall proxy. proxy responseList -> Variant response -> Status

instance responseVariantToStatusNil :: ResponseVariantToStatus response Nil where
  -- | This will never be reached if the data originates from PureScript.
  responseVariantToStatus _ _ = unsafeCrashWith "Unmatchable response variant"

instance responseVariantToStatusCons ::
  ( IsSymbol status
  , ResponseStatus status
  , Cons status body response' response
  , ResponseVariantToStatus response responseList
  ) =>
  ResponseVariantToStatus response (Cons status body responseList) where
  responseVariantToStatus _ response =
    if isJust (prj (SProxy :: _ status) response) then
      Status.toStatus (SProxy :: _ status)
    else
      responseVariantToStatus (RLProxy :: _ responseList) response
