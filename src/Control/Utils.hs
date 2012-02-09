module Control.Utils where

ifM :: (Monad m) => m Bool -> m a -> m a -> m a
ifM cond t f = cond >>= \c -> if c then t else f
