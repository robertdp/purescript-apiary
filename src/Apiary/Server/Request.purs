module Apiary.Server.Request where

import Prelude
import Apiary.Media (class MediaCodec, decodeMedia)
import Apiary.Route (class PrepareSpec, Route)
import Apiary.Server.Url (class ReadParams, PathParams, QueryParams, readParams)
import Data.Either (Either(..), either)
import Effect.Aff (Aff, launchAff_, throwError)
import Effect.Aff.AVar as AVar
import Effect.Class (liftEffect)
import Effect.Exception (catchException)
import Foreign (F)
import Foreign.Object (Object)
import Foreign.Object as Object
import Node.Buffer (Buffer)
import Node.Buffer as Buffer
import Node.Encoding as Encoding
import Node.HTTP as HTTP
import Node.Stream as Stream
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

readBodyAsBuffer :: HTTP.Request -> Aff Buffer
readBodyAsBuffer request = do
  let
    stream = HTTP.requestAsStream request
  bodyResult <- AVar.empty
  chunks <- AVar.new []
  fillResult <-
    liftEffect
      $ catchException (pure <<< Left) (Right <$> fillBody stream chunks bodyResult)
  body <- AVar.take bodyResult
  either throwError pure (fillResult *> body)
  where
  fillBody stream chunks bodyResult = do
    Stream.onData stream \chunk ->
      let
        modification = do
          v <- AVar.take chunks
          AVar.put (v <> [ chunk ]) chunks
      in
        launchAff_ modification
    Stream.onError stream (launchAff_ <<< flip AVar.put bodyResult <<< Left)
    Stream.onEnd stream do
      launchAff_ do
        AVar.take chunks
          >>= concat'
          >>= (pure <<< Right)
          >>= flip AVar.put bodyResult

  concat' = liftEffect <<< Buffer.concat

readBodyAsString :: HTTP.Request -> Aff String
readBodyAsString = readBodyAsBuffer >=> liftEffect <<< Buffer.toString Encoding.UTF8
