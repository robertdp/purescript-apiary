module Apiary.Server.Handler
  ( Handler(..)
  , runHandler
  , module Ix
  ) where

import Prelude
import Control.Applicative.Indexed (class IxApplicative, iapply, imap, ipure)
import Control.Apply.Indexed (class IxApply)
import Control.Bind.Indexed (class IxBind, ibind)
import Control.Monad.Indexed (class IxMonad)
import Control.Monad.Indexed.Qualified (apply, bind, discard, map, pure) as Ix
import Control.Monad.Indexed.Trans (class IxMonadTrans)
import Control.Monad.Indexed.Trans.Qualified (lift) as Ix
import Data.Functor.Indexed (class IxFunctor)
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

instance ixMonadTransHandler :: Monad m => IxMonadTrans Handler where
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
