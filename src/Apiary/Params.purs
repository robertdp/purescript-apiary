module Apiary.Params where

import Prelude
import Data.String (Pattern(..), Replacement(..))
import Data.String as String
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Global.Unsafe (unsafeEncodeURIComponent)
import Prim.Row (class Cons)
import Prim.RowList (class RowToList, Cons, Nil)
import Record as Record
import Type.Data.RowList (RLProxy(..))
import Unsafe.Coerce (unsafeCoerce)

class EncodeParam param where
  encodeParam :: param -> String

instance encodeParamInt :: EncodeParam Int where
  encodeParam = show

instance encodeParamString :: EncodeParam String where
  encodeParam = unsafeEncodeURIComponent

class WriteParams params where
  writeParams :: params -> String -> String

class WriteParamRecord params paramList | paramList -> params where
  writeParamRecord :: RLProxy paramList -> Record params -> String -> String

instance writeParamsUnit :: WriteParams Unit where
  writeParams _ = identity

instance writeParamsRecord ::
  ( RowToList params paramList
  , WriteParamRecord params paramList
  ) =>
  WriteParams { | params } where
  writeParams = writeParamRecord (RLProxy :: _ paramList)

instance writeParamRecordNil :: WriteParamRecord () Nil where
  writeParamRecord _ _ = identity

instance writeParamRecordCons ::
  ( Cons name param params' params
  , IsSymbol name
  , EncodeParam param
  , WriteParamRecord params' paramList
  ) =>
  WriteParamRecord params (Cons name param paramList) where
  writeParamRecord _ params url = writeParamRecord (RLProxy :: _ paramList) (unsafeCoerce params) modifiedUrl
    where
    modifiedUrl = String.replace (Pattern pattern) (Replacement replacement) url

    pattern = "<" <> reflectSymbol (SProxy :: _ name) <> ">"

    replacement = encodeParam $ Record.get (SProxy :: _ name) params
