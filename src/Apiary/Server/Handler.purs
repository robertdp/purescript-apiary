module Apiary.Server.Handler
  ( Handler(..)
  , runHandler
  , module Ix
  ) where

import Prelude
import Control.Applicative.Indexed (class IxApplicative, iapply, imap, ipure)
import Control.Apply.Indexed (class IxApply)
import Control.Bind.Indexed (class IxBind, ibind)
import Control.Monad.Error.Class (class MonadError, class MonadThrow, catchError, throwError)
import Control.Monad.Indexed (class IxMonad)
import Control.Monad.Indexed.Qualified (apply, bind, discard, map, pure) as Ix
import Control.Monad.Indexed.Trans (class IxMonadTrans)
import Control.Monad.Indexed.Trans.Qualified (lift) as Ix
import Data.Functor.Indexed (class IxFunctor)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Node.HTTP (Response)

newtype Handler m from to a
  = Handler (Response -> m a)

runHandler :: forall m from to a. Handler m from to a -> Response -> m a
runHandler (Handler f) = f

instance ixFunctorHandler :: Monad m => IxFunctor (Handler m) where
  imap f a = Handler \res -> map f (runHandler a res)

instance ixApplyHandler :: Monad m => IxApply (Handler m) where
  iapply f a = Handler \res -> apply (runHandler f res) (runHandler a res)

instance ixApplicativeHandler :: Monad m => IxApplicative (Handler m) where
  ipure a = Handler \res -> pure a

instance ixBindHandler :: Monad m => IxBind (Handler m) where
  ibind ma f =
    Handler \res -> do
      a <- runHandler ma res
      case f a of
        Handler k -> k res

instance ixMonadHandler :: Monad m => IxMonad (Handler m)

instance ixMonadTransHandler :: IxMonadTrans Handler where
  ilift ma = Handler \_ -> ma

instance functorHandler :: Monad m => Functor (Handler m x x) where
  map = imap

instance applyHandler :: Monad m => Apply (Handler m x x) where
  apply = iapply

instance applicativeHandler :: Monad m => Applicative (Handler m x x) where
  pure = ipure

instance bindHandler :: Monad m => Bind (Handler m x x) where
  bind = ibind

instance monadHandler :: Monad m => Monad (Handler m x x)

instance monadEffectHandler :: MonadEffect m => MonadEffect (Handler m x x) where
  liftEffect ma = Handler \_ -> liftEffect ma

instance monadAffHandler :: MonadAff m => MonadAff (Handler m x x) where
  liftAff ma = Handler \_ -> liftAff ma

instance monadThrowHandler :: MonadThrow e m => MonadThrow e (Handler m x x) where
  throwError err = Handler \_ -> throwError err

instance monadErrorHandler :: MonadError e m => MonadError e (Handler m x x) where
  catchError ma f = Handler \res -> catchError (runHandler ma res) (\err -> case f err of mb -> runHandler mb res)
