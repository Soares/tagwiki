{-# Language FlexibleContexts #-}
module Data.State where
import Control.Applicative hiding ( empty )
import Control.Dangerous
import Control.Monad.State hiding ( State )
import Data.Cache
import Data.Trail
import qualified Data.Record as Record
import {-# SOURCE #-} Control.DateTime.Moment
import Text.Pinpoint
import Data.File

data State = State
    { cache :: Cache
    , trail :: Trail
    }

modifyCache :: (MonadState State m) => (Cache -> Cache) -> m ()
modifyCache fn = modify $ \st -> st{cache=fn $ cache st}

modifyTrail :: (MonadState State m) => (Trail -> Trail) -> m ()
modifyTrail fn = modify $ \st -> st{trail=fn $ trail st}

getEra :: (MonadState State m) => m (Maybe String)
getEra = gets (currentEra . trail)
pushEra :: (MonadState State m) => String -> m ()
pushEra e = modifyTrail $ descendEra e
popEra :: (MonadState State m) => m ()
popEra = modifyTrail ascendEra

getFile :: (MonadState State m) => m (Maybe File)
getFile = gets (currentFile . trail)
pushRef :: (MonadState State m, Applicative m) => Pinpoint -> File -> m ()
pushRef p f = modifyTrail (descendRef p) *> modifyTrail (descendFile f)
popRef :: (MonadState State m, Applicative m) => m ()
popRef = modifyTrail ascendRef *> modifyTrail ascendFile
unSelf :: (MonadState State m, Applicative m) => Pinpoint -> m Pinpoint
unSelf p = handle <$> getFile where
    handle = maybe p (flip setPin p . Record.pin)

guard :: (MonadState State m, Errorable m, Show e) => (Trail -> e) -> m ()
guard err = do
    trl <- gets trail
    unless (verify trl) (throw $ err trl)

cached :: (MonadState State m, Applicative m, Ord k)
                                => (k -> Cache -> Maybe a)
                                -> (k -> a -> Cache -> Cache)
                                -> k -> m a -> m a
cached look set k ma = maybe (send set k ma) pure =<< look k <$> gets cache

send :: (MonadState State m, Applicative m, Ord k)
    => (k -> a -> Cache -> Cache) -> k -> m a -> m a
send set k ma = do
    a <- ma
    modifyCache (set k a)
    pure a

cachedLocation :: (MonadState State m, Applicative m) =>
    Pinpoint -> m String -> m String
cachedLocation p ma = do
    p' <- unSelf p
    cached getLocation putLocation p' ma
cachedMoment :: (MonadState State m, Applicative m) =>
    Pinpoint -> m Moment -> m Moment
cachedMoment p ma = do
    p' <- unSelf p
    cached getMoment putMoment p' ma
cachedOffset :: (MonadState State m, Applicative m) =>
    String -> m (Maybe (Direction, Moment)) ->
    m (Maybe (Direction, Moment))
cachedOffset = cached getOffset putOffset

clean :: State
clean = State empty home
