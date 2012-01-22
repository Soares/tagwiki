module Control.Utils where

aapply :: (Monad m) => (a -> b -> m c) -> m a -> m b -> m c
aapply fn ma mb = ma >>= ((mb >>=) . fn)
