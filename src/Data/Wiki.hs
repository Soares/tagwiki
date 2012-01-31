{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module Data.Wiki
    ( Wiki
    , new
    -- , record
    -- , recordEra
    -- , recordPlace
    -- , recordCharacter
    , tagList
    , build
    ) where
import Context hiding ( doWithEra, doWithPinpoint )
import Control.Arrow
import Control.Applicative
import Control.Dangerous ( Dangerous, warn, Errorable )
import Control.Dangerous.Extensions()
import Control.DateTime.Moment ( Moment, Direction, Offset, present )
import Control.DateTime.Moment ( Offset(..) ) -- TODO
import Control.Monad.Reader ( MonadReader, ReaderT, asks, )
import Control.Monad.State ( StateT )
import Control.Name ( priorities )
import Data.Body ( event )
import Data.File ( File(File) )
import Data.Function ( on )
import Data.List ( intercalate, sort )
import Data.Map ( Map )
import Data.Maybe ( fromMaybe )
import Data.Utils ( headOr )
import Internal ( Internal(..) )
import Note ( Note(pins, tags, uid, text, body) )
-- import Note.Character
-- import Note.Era
-- import Note.Place
import Prelude hiding ( log )
import Control.Event hiding ( text )
import Text.Pin ( Pin )
import Text.Point ( Side(Auto), side )
import Text.Pinpoint ( Pinpoint, pin, point )
import Text.Printf ( printf )
import Text.Render ( link, href )
import qualified Context
import qualified Data.Body as Body
import qualified Data.Map as Map
import qualified Data.Set as Set
import {-# SOURCE #-} Note.Era ( Era, dawn )


-- | Operations that can poke around the directory
instance (Errorable m, Applicative m, Monad m, MonadReader Wiki m) =>
    Internal (StateT Context (ReaderT Wiki m)) where
    doWithEra = Context.doWithEra
    doWithPinpoint = Context.doWithPinpoint

    lookupEraCode str = cacheOffset str create where
        create = maybe cantFind lookup . Map.lookup str =<< asks eras
        lookup (side, era) = Just <$> pure Root -- TODO: call dawn, make an era
        cantFind = warn (NoSuchEra str) *> pure Nothing

    pinpoint p = cachePinpoint p create where
        create = doWithPinpoint p (pure present)
        -- TODO: don't use Data.Body.
        -- Add an 'event' method to Note.
        {-
        momentus = case event (point p) (body $ find $ pin p) of
            Nothing -> forceMaybe Nothing
            Just ev -> at (maybe Auto side $ point p) ev >>= forceMaybe
        forceMaybe Nothing = warn (Unknown p) *> pure present
        forceMaybe (Just x) = pure x
        -}


class (MonadReader Wiki o, Contextual o, Internal o) => Operational o

-- | The structure of a wiki
-- | TODO: Rename to Wiki
data Wiki = Wiki
    { listing :: Map FilePath File
    -- , places  :: Map Place Pin
    , eras    :: Map String (Direction, Era)
    }


new :: Wiki
new = Wiki Map.empty Map.empty -- Map.empty

{-
-- record :: (forall a. Note a) => Wiki -> FilePath -> a -> Wiki
record w n f = w{ listing = Map.insert n (File f) (listing w) }

recordEra :: Wiki -> FilePath -> Era -> Wiki
recordEra w n e = (record w n e){ eras = update (eras w) } where
    update dict = foldr insert dict (codes e)
    insert (code, dir) = Map.insert code (dir, e)

recordPlace :: Wiki -> FilePath -> Place -> Wiki
recordPlace w n p = (record w n p){ places = update (places w) } where
    update = Map.insert p (parent p)

recordCharacter :: Wiki -> FilePath -> Character -> Wiki
recordCharacter = record
-}

-- Directory building

-- | All the maps of pins on to files, in order of priority
maps :: Wiki -> [Map Pin [File]]
maps w = map (pinMap $ listing w) priorities where
    pinMap dir pri = Map.fold (addKeys pri) Map.empty dir
    addKeys pri file dict = foldr (addKey file) dict (pins pri file)
    addKey file pin = Map.insertWith (++) pin [file]


-- | The list of all tags for all files (sorted)
tagList :: Wiki -> [(FilePath, String)]
tagList = convertToList . expandToTags . listing where
    convertToList = sort . concatMap expand . Map.toList
    expandToTags = Map.map (Set.toList . tags)
    expand (filename, tags) = zip (repeat filename) tags


-- | An href for a pinpoint.
-- | Defaults to an empty string.
location :: (MonadReader Wiki c, Contextual c) => Pinpoint -> c String
location pp = maybe "" fromFile <$> find p where
    (p, pt) = (pin &&& point) pp
    fromFile = link <$> to <*> show
    to = href (show <$> pt) . uid


-- | Find a pin.
-- | Uses the current file if the pin is empty.
-- |    (The context will fill empty pins for us)
-- | Logs warnings the first time a pin is looked up, but not subsequently:
-- |    Logs a warning if the pin isn't found.
-- |    Logs a warning if the pin is ambiguous.
find :: (MonadReader Wiki c, Contextual c) => Pin -> c (Maybe File)
find p = cachePin p (doFirst =<< candidates) where
    candidates = headOr [] . filter (not . null) <$> candidateLists
    candidateLists = map dictCandidates <$> asks maps
    dictCandidates = fromMaybe [] . Map.lookup p
    doFirst [] = warn (NotFound p) *> pure Nothing
    doFirst [x] = pure $ Just x
    doFirst (x:xs) = warn (Ambiguous p $ x:xs) *> doFirst [x]


-- | Executes a function on each file, passing it the file uid and contents.
-- | Designed for i.e. (writefile . (dir </>))
build :: (MonadReader Wiki c, Contextual c, Internal c) =>
            (FilePath -> String -> c a) -> c [a]
build fn = mapM (>>= uncurry fn) . filePairs =<< asks listing where
    filePairs dict = map makePair (Map.elems dict)
    makePair f = (,) (uid f) <$> text f


-- | Error handling
-- | (And warning handling)

data Warning
    = NotFound Pin
    | Ambiguous Pin [File]
    | Unknown Pinpoint
    | NoSuchEra String
instance Show Warning where
    show (NotFound p) = printf "Can't find '%s'" $ show p
    show (Unknown p) = printf "Unknown moment: '%s'" $ show p
    show (NoSuchEra e) = printf "No such era: %s" e
    show (Ambiguous p fs) = printf "Ambiguities looking for %s\n\t%s"
        (show p) (intercalate "\n\t" $ map show fs)
