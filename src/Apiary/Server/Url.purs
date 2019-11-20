module Apiary.Server.Url where

import Prelude

import Apiary.Url (class UrlParam, decodeUrlParam, encodeUrlParam)
import Control.Alt ((<|>))
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except (withExceptT)
import Data.Array as Array
import Data.Maybe (Maybe(..), maybe)
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Data.Traversable (sequence)
import Foreign (F, Foreign, ForeignError(..))
import Foreign.Object (Object)
import Foreign.Object as Object
import Node.URL as URL
import Prim.Row (class Cons, class Lacks)
import Prim.RowList (class RowToList, Nil, Cons)
import Record.Builder (Builder)
import Record.Builder as Builder
import Simple.JSON (class ReadForeign, read', readJSON')
import Type.Data.RowList (RLProxy(..))
import Type.Proxy (Proxy)
import Unsafe.Coerce (unsafeCoerce)

type PathParams
  = Object String

type QueryParams
  = Object Foreign

coerceQuery :: URL.Query -> QueryParams
coerceQuery = unsafeCoerce

class DecodePathParams params where
  decodePathParams :: PathParams -> F params

instance decodePathParamsRecord ::
  ( RowToList r rl
  , DecodePathParamRecord r rl
  ) =>
  DecodePathParams (Record r) where
  decodePathParams =
    decodePathParamRecord (RLProxy :: _ rl)
      >>> map (flip Builder.build {})

class DecodePathParamRecord r rl | rl -> r where
  decodePathParamRecord :: RLProxy rl -> PathParams -> F (Builder {} (Record r))

instance decodePathParamRecordNil :: DecodePathParamRecord () Nil where
  decodePathParamRecord _ _ = pure identity

instance decodePathParamRecordConsString ::
  ( Cons l String r_ r
  , DecodePathParamRecord r_ rl_
  , IsSymbol l
  , Lacks l r_
  ) =>
  DecodePathParamRecord r (Cons l String rl_) where
  decodePathParamRecord _ f = do
    builder <- decodePathParamRecord (RLProxy :: RLProxy rl_) f
    let
      l = reflectSymbol (SProxy :: SProxy l)
    a <- maybe (throwError (pure (ErrorAtProperty l (ForeignError "missing param")))) pure (Object.lookup l f)
    pure (builder >>> Builder.insert (SProxy :: SProxy l) a)
else instance decodePathParamRecordCons ::
  ( Cons l a r_ r
  , DecodePathParamRecord r_ rl_
  , IsSymbol l
  , Lacks l r_
  , ReadForeign a
  ) =>
  DecodePathParamRecord r (Cons l a rl_) where
  decodePathParamRecord _ f = do
    builder <- decodePathParamRecord (RLProxy :: RLProxy rl_) f
    let
      l = reflectSymbol (SProxy :: SProxy l)
    f_ <- maybe (throwError (pure (ErrorAtProperty l (ForeignError "missing param")))) pure (Object.lookup l f)
    a <- withExceptT (map (ErrorAtProperty l)) (readJSON' f_)
    pure (builder >>> Builder.insert (SProxy :: SProxy l) a)

class DecodeQueryParams rep params | rep -> params where
  decodeQueryParams :: Proxy rep -> QueryParams -> F params

instance decodeQueryParamsRecord ::
  ( RowToList queryRep queryList
  , DecodeQueryParamRecord query queryList
  ) =>
  DecodeQueryParams (Record queryRep) (Record query) where
  decodeQueryParams _ =
    decodeQueryParamRecord (RLProxy :: _ queryList)
      >>> map (flip Builder.build {})

class DecodeQueryParamRecord r rl | rl -> r where
  decodeQueryParamRecord :: RLProxy rl -> QueryParams -> F (Builder {} (Record r))

instance decodeQueryParamRecordNil :: DecodeQueryParamRecord () Nil where
  decodeQueryParamRecord _ _ = pure identity

instance decodeQueryParamRecordConsArray ::
  ( IsSymbol name
  , UrlParam value
  , Cons name (Array value) params' params
  , Lacks name params'
  , DecodeQueryParamRecord params' paramList
  ) =>
  DecodeQueryParamRecord params (Cons name (Array value) paramList) where
  decodeQueryParamRecord _ params = do
    let
      name = SProxy :: _ name

      prop = encodeUrlParam $ reflectSymbol name
    builder <- decodeQueryParamRecord (RLProxy :: _ paramList) params
    value <- case Object.lookup prop params of
      Nothing -> pure []
      Just a -> do
        values <- read' a <|> Array.singleton <$> read' a
        sequence $ decodeUrlParam <$> values
    pure $ Builder.insert name value <<< builder
else instance decodeQueryParamRecordCons ::
  ( IsSymbol name
  , UrlParam value
  , Cons name (Maybe value) params' params
  , Lacks name params'
  , DecodeQueryParamRecord params' paramList
  ) =>
  DecodeQueryParamRecord params (Cons name (Maybe value) paramList) where
  decodeQueryParamRecord _ params = do
    let
      name = SProxy :: _ name

      prop = encodeUrlParam $ reflectSymbol name
    builder <- decodeQueryParamRecord (RLProxy :: _ paramList) params
    value <- case Object.lookup prop params of
      Nothing -> pure Nothing
      Just a -> do
        str <- read' a
        Just <$> decodeUrlParam str
    pure $ Builder.insert name value <<< builder
