module Apiary.Server.Response
  ( Response(..)
  , runResponse
  , StatusLineOpen
  , HeadersOpen
  , BodyOpen
  , ResponseEnded
  , Header
  , FullResponse
  , writeStatus
  , writeHeader
  , closeHeaders
  , headers
  , contentType
  , withResponseStream
  , send
  , respondWithMedia
  , module Ix
  ) where

import Prelude
import Apiary.Media (class MediaCodec, encodeMedia, mediaType)
import Apiary.Status (Status(..))
import Control.Applicative.Indexed (class IxApplicative, iapply, imap, ipure)
import Control.Apply.Indexed (class IxApply)
import Control.Bind.Indexed (class IxBind, ibind)
import Control.Monad.Error.Class (class MonadError, class MonadThrow, catchError, throwError)
import Control.Monad.Indexed (class IxMonad)
import Control.Monad.Indexed.Qualified (apply, bind, discard, map, pure) as Ix
import Control.Monad.Indexed.Trans (class IxMonadTrans)
import Control.Monad.Indexed.Trans.Qualified (lift) as Ix
import Data.Foldable (class Foldable, traverse_)
import Data.Functor.Indexed (class IxFunctor)
import Data.MediaType (MediaType)
import Data.Newtype (unwrap)
import Data.Tuple (Tuple, uncurry)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Node.Encoding as Encoding
import Node.HTTP as HTTP
import Node.Stream as Stream
import Type.Proxy (Proxy)

newtype Response m from to a
  = Response (HTTP.Response -> m a)

runResponse :: forall m from to a. Response m from to a -> HTTP.Response -> m a
runResponse (Response f) = f

instance ixFunctorResponse :: Monad m => IxFunctor (Response m) where
  imap f a = Response \res -> map f (runResponse a res)

instance ixApplyResponse :: Monad m => IxApply (Response m) where
  iapply f a = Response \res -> apply (runResponse f res) (runResponse a res)

instance ixApplicativeResponse :: Monad m => IxApplicative (Response m) where
  ipure a = Response \res -> pure a

instance ixBindResponse :: Monad m => IxBind (Response m) where
  ibind ma f =
    Response \res -> do
      a <- runResponse ma res
      case f a of
        Response k -> k res

instance ixMonadResponse :: Monad m => IxMonad (Response m)

instance ixMonadTransResponse :: IxMonadTrans Response where
  ilift ma = Response \_ -> ma

instance functorResponse :: Monad m => Functor (Response m x x) where
  map = imap

instance applyResponse :: Monad m => Apply (Response m x x) where
  apply = iapply

instance applicativeResponse :: Monad m => Applicative (Response m x x) where
  pure = ipure

instance bindResponse :: Monad m => Bind (Response m x x) where
  bind = ibind

instance monadResponse :: Monad m => Monad (Response m x x)

instance monadEffectResponse :: MonadEffect m => MonadEffect (Response m x x) where
  liftEffect ma = Response \_ -> liftEffect ma

instance monadAffResponse :: MonadAff m => MonadAff (Response m x x) where
  liftAff ma = Response \_ -> liftAff ma

instance monadThrowResponse :: MonadThrow e m => MonadThrow e (Response m x x) where
  throwError err = Response \_ -> throwError err

instance monadErrorResponse :: MonadError e m => MonadError e (Response m x x) where
  catchError ma f = Response \res -> catchError (runResponse ma res) (\err -> case f err of mb -> runResponse mb res)

data StatusLineOpen

data HeadersOpen

data BodyOpen

data ResponseEnded

type Header
  = Tuple String String

type FullResponse m
  = Response m StatusLineOpen ResponseEnded Unit

writeStatus :: forall m. MonadEffect m => Status -> Response m StatusLineOpen HeadersOpen Unit
writeStatus (Status { code, reason }) =
  Response \res ->
    liftEffect do
      HTTP.setStatusCode res code
      HTTP.setStatusMessage res reason
      HTTP.setHeader res "Access-Control-Allow-Origin" "*"

writeHeader :: forall m. MonadEffect m => String -> String -> Response m HeadersOpen HeadersOpen Unit
writeHeader name value =
  Response \res ->
    liftEffect do
      HTTP.setHeader res name value

closeHeaders :: forall m. Monad m => Response m HeadersOpen BodyOpen Unit
closeHeaders = Response \_ -> pure unit

headers :: forall f m. Foldable f => MonadEffect m => f Header -> Response m HeadersOpen BodyOpen Unit
headers hs = Ix.do
  traverse_ (uncurry writeHeader) hs
  closeHeaders

contentType :: forall m. MonadEffect m => MediaType -> Response m HeadersOpen HeadersOpen Unit
contentType mediaType = writeHeader "Content-Type" (unwrap mediaType)

withResponseStream ::
  forall m a.
  MonadEffect m =>
  (Stream.Writable () -> m a) ->
  Response m BodyOpen ResponseEnded a
withResponseStream f =
  Response \res -> do
    let
      s = HTTP.responseAsStream res
    a <- f s
    liftEffect do Stream.end s mempty
    pure a

send :: forall m. MonadEffect m => String -> Response m BodyOpen ResponseEnded Unit
send str =
  withResponseStream \stream ->
    void
      $ liftEffect do
          Stream.writeString stream Encoding.UTF8 str mempty

respondWithMedia ::
  forall m rep a.
  MediaCodec rep a =>
  MonadEffect m =>
  Status ->
  Proxy rep ->
  a ->
  FullResponse m
respondWithMedia status rep response = Ix.do
  writeStatus status
  traverse_ contentType (mediaType rep)
  closeHeaders
  send (encodeMedia rep response)
