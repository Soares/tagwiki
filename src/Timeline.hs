{-# Language FlexibleContexts #-}
module Timeline where
import Control.Applicative hiding ( (<|>) )
import Control.Dangerous hiding ( die )
import Control.DateTime.Absolute hiding ( normalize )
import Control.Monad.Reader hiding ( when )
import Data.Wiki
import Internal
import Note.Era
import qualified Data.Map as Map
import Text.Point ( Side(Auto) )
import Text.Printf
import Text.Utils hiding ( normalize )

{-
data Timeline = Timeline
    { id            :: String
    , title         :: String
    , description   :: Maybe String
    , focus_date    :: Maybe Moment
    , initialZoom   :: Maybe Int
    , events        :: [Event]
    , legend        :: [Legend]
    }

data Legend = Legend
    { title         :: String
    , icon          :: URL
    }

data Event = Event
    { id            :: String
    , title         :: String
    , description   :: Maybe String
    , startDate     :: Moment
    , endDate       :: Maybe Moment
    , dateDisplay   :: Maybe String
    , link          :: URL
    , icon          :: Maybe URL
    , importance    :: Int
    -- Map not supported
    }

timelines :: Directory -> [Timeline]
-}

-- TODO: relax
lookupEra :: (Internal i, MonadReader Wiki i) => String -> i (Maybe Era)
lookupEra str = Map.lookup str <$> asks eras

normalize :: Absolute -> Era -> Absolute
normalize Present _ = Present
normalize a e | slugify (era a) `elem` precodes e = invert (code e) a
              | otherwise = a{era=code e}

rooted :: (Internal i, MonadReader Wiki i) => Era -> i Absolute
rooted e = handle =<< when Auto e where
    handle Present = pure $ Absolute (code e) 0 0 0 0 0 0 0
    handle a = maybe (die a) (recurse $ toRel a) =<< lookupEra (era a)
    recurse a e' = flip plus a <$> rooted e'
    die a = warn (EraNotFound $ era a) *> pure a

convertEra :: (Internal i, MonadReader Wiki i) => Absolute -> String -> i Absolute
convertEra Present _ = pure Present
convertEra a e | era a == e = pure a
convertEra a e = do
    mea <- lookupEra (era a)
    meb <- lookupEra e
    case (mea, meb) of
        (Nothing, _) -> warn (EraNotFound $ era a) *> pure a{era=e}
        (Just _, Nothing) -> warn (EraNotFound e) *> pure a
        (Just ea, Just eb) -> do
            let a' = normalize a ea
            let die = warn (CantRelate ea eb) *> pure a'
            mda <- diff <$> rooted ea <*> rooted eb
            maybe die (pure . plus a') mda

data Error = EraNotFound String | CantRelate Era Era
instance Show Error where
    show (EraNotFound e) = printf "Can't find era %s" e
    show (CantRelate a b) = printf "Can't relate era %s to %s" (show a) (show b)
