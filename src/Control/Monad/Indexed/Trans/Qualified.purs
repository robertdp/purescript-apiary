module Control.Monad.Indexed.Trans.Qualified where

import Control.Monad (class Monad)
import Control.Monad.Indexed.Trans (class IxMonadTrans, ilift)

lift :: forall m i a t. IxMonadTrans t => Monad m => m a -> t m i i a
lift = ilift
