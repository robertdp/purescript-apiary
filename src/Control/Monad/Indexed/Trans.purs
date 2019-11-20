module Control.Monad.Indexed.Trans where

import Control.Monad (class Monad)

class IxMonadTrans t where
  ilift :: forall m i a. Monad m => m a -> t m i i a
