module Apiary.Server.Request where

import Prelude
import Apiary.Media (class MediaCodec, decodeMedia)
import Apiary.Route (class PrepareSpec, Route)
import Apiary.Server.Url (class ReadParams, PathParams, QueryParams, readParams)
import Foreign (F)
import Foreign.Object (Object)
import Foreign.Object as Object
import Type.Proxy (Proxy(..))

type Request params body
  = { params :: params
    , headers :: Object String
    , body :: body
    }

class DecodeRequest route params body | route -> params body where
  decodeRequest :: route -> PathParams -> QueryParams -> String -> F (Request params body)

instance decodeRequestRoute ::
  ( PrepareSpec
      spec
      { params :: params
      , query :: query
      , body :: body
      , response :: response
      }
  , ReadParams params query fullParams
  , MediaCodec body body'
  ) =>
  DecodeRequest (Route path method spec) fullParams body' where
  decodeRequest _ pathParams queryParams requestBody = do
    params <- readParams (Proxy :: _ params) (Proxy :: _ query) pathParams queryParams
    body <- decodeMedia (Proxy :: _ body) requestBody
    pure { params, headers: Object.empty, body }
