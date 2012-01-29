{-# Language FlexibleContexts #-}
module Data.State where
import Control.Monad.State hiding ( State, guard )
import Control.Dangerous
import {-# SOURCE #-} Data.Trail ( Trail )

data State

getEra :: (MonadState State m) => m (Maybe String)
pushEra :: (MonadState State m) => String -> m ()
popEra :: (MonadState State m) => m ()
guard :: (MonadState State m, Errorable m, Show e) => (Trail -> e) -> m ()
