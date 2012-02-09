{-# Language FlexibleContexts #-}
{-# Language FlexibleInstances #-}
module Context ( Context, Contextual(..), clean ) where
import Control.Applicative hiding ( empty )
import Control.Dangerous hiding ( Warning )
import Control.Dangerous.Extensions()
import Control.DateTime.Absolute
import Control.Monad.State hiding ( guard )
import Internal
import Text.Pin ( Pin )
import Text.Point ( Point )
import Context.Cache
import Context.Trail
import Data.File

data Context = Context
    { cache :: Cache
    , trail :: Trail
    }

clean :: Context
clean = Context empty home


class (Errorable m, MonadState Context m, Applicative m) => Contextual m where
    cacheOffset :: String -> m (Maybe (Direction, Absolute)) -> m (Maybe (Direction, Absolute))
    cacheOffset = cached getCode putCode

    doWithEra :: String -> m a -> m a
    doWithEra str fn = pushEra str *> guard Cycle *> fn <* popEra

    cacheRef :: File -> Maybe Point -> m Absolute -> m Absolute
    cacheRef p pt = cached getRef putRef (p, pt)

    doWithRef :: File -> Maybe Point -> m a -> m a
    doWithRef p pt fn = pushPP p pt *> guard Cycle *> fn <* popPP

    currentFile :: m (Maybe File)
    currentFile = maybe (pure Nothing) justFile =<< ref where
        justFile = pure . Just . fst
        ref = gets $ currentRef . trail

    cachePin :: Pin -> m (Maybe File) -> m (Maybe File)
    cachePin = cached getPin putPin

    -- Pin cycles do not bother us, as there may be different references
    -- to different things in the same file. doWithPinpoint handles that
    -- case already.
instance (Errorable m, Applicative m) => Contextual (StateT Context m)


-- | Generic state manipulation functions
modifyTrail :: (MonadState Context m) => (Trail -> Trail) -> m ()
modifyTrail fn = modify $ \ctx -> ctx{trail=fn $ trail ctx}

modifyCache :: (Contextual m) => (Cache -> Cache) -> m ()
modifyCache fn = modify $ \ctx -> ctx{cache=fn $ cache ctx}

guard :: (Contextual m, Show e) => (Trail -> e) -> m ()
guard err = gets trail >>= \t -> unless (verifyTrail t) (throw $ err t)

cached :: (Contextual m, Ord k) =>
          (k -> Cache -> Maybe a) ->        -- Getter
          (k -> a -> Cache -> Cache) ->     -- Setter
          k -> m a -> m a                   -- Key -> Operation -> Result
cached look set k ma = maybe send pure =<< look k <$> gets cache where
    send = do
        a <- ma
        modifyCache (set k a)
        pure a


-- | Era recursion helpers
pushEra :: (Contextual m) => String -> m ()
pushEra e = modifyTrail $ descendEra e
popEra :: (Contextual m) => m ()
popEra = modifyTrail ascendEra


-- | Pinpoint recursion helpers
pushPP :: (Contextual m) => File -> Maybe Point -> m ()
pushPP p pt = modifyTrail $ descendRef (p, pt)
popPP :: (Contextual m) => m ()
popPP = modifyTrail ascendRef


data Error = Cycle Trail
instance Show Error where
    show (Cycle t) = "References form cycle:\n\t" ++ show t

data Warning = CantUnself Pin
instance Show Warning where
    show (CantUnself p) =
        "Tried to resolve local pin " ++ show p ++ " while not in context."
