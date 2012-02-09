{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Data.Wiki
    ( Wiki(..)
    , new
    , runInternal
    , record
    , recordEra
    , recordPlace
    , recordCharacter
    , remap
    , tagList
    , build
    ) where
import Context
import Control.Arrow
import Control.Applicative
import Control.Dangerous ( warn, Errorable )
import Control.Dangerous.Extensions()
import Control.DateTime.Absolute
import Control.Monad.Reader ( ReaderT(..), asks, )
import Control.Monad.State ( StateT(..) )
import Control.Name ( priorities, ofPriority )
import Data.File ( File(File) )
import Data.List ( intercalate, sort )
import Data.Map ( Map )
import Data.Utils ( headOr, thread )
import Internal ( Internal(..) )
import Note ( Note(tags, uid, pointer, names, recognizes) )
import Note.Era ( Era, codes, precodes )
import Note.Character ( Character )
import Note.Place ( Place, parent )
import Prelude hiding ( log )
import Text.Pin ( Pin, isSelf )
import Text.Pinpoint ( Pinpoint, pin, point )
import Text.Printf ( printf )
import Text.Render ( link, href )
import Text.Utils
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Text.Pin as Pin

runInternal :: StateT Context (ReaderT Wiki IO) a -> Wiki -> IO a
runInternal fn = fmap fst . runReaderT (runStateT fn clean)

-- | Operations that can poke around the directory
-- | TODO: relax constraints, use classes like Errorable/MonadState
instance Internal (StateT Context (ReaderT Wiki IO)) where
    -- Resolve a pinpoint to a moment.
    pinpoint p = maybe (pure Present) resolveToMoment =<< find (pin p) where
        resolveToMoment file = cacheRef file pt (momentus file)
        momentus file = doWithRef file pt (pointIn file)
        pointIn = (forceMaybe =<<) . pointer pt
        forceMaybe Nothing = warn (Unknown p) *> pure Present
        forceMaybe (Just x) = pure x
        pt = point p

    -- | An href for a pinpoint.
    -- | Defaults to an empty string.
    location pp = maybe "" fromFile <$> find p where
        (p, pt) = (pin &&& point) pp
        fromFile = link <$> to <*> show
        to = href (show <$> pt) . show


    -- | Find a pin.
    -- | Uses the current file if the pin is empty.
    -- |    (The context will fill empty pins for us)
    -- | Logs warnings the first time a pin is looked up, but not subsequently:
    -- |    Logs a warning if the pin isn't found.
    -- |    Logs a warning if the pin is ambiguous.
    find p | isSelf p = currentFile
           | otherwise = cachePin p (doFirst =<< candidates) where
        doFirst [] = warn (NotFound p) *> pure Nothing
        doFirst [x] = pure $ Just x
        doFirst (x:xs) = warn (Ambiguous p $ x:xs) *> doFirst [x]
        candidates = headOr [] . filter (not . null) <$> candidateLists
        candidateLists = map dictCandidates <$> asks maps
        dictCandidates = concatMap recognizers . Map.toList
        recognizers (t, fs) | Pin.tag p == t = filter (recognizes p) fs
                            | otherwise = []


    -- | Executes a function on each file, passing it the file uid and contents.
    -- | Designed for i.e. (writefile . (dir </>))
    build fn = mapM (uncurry fn) . filePairs =<< asks listing where
        filePairs dict = map makePair (Map.elems dict)
        makePair f = (,) (slugify (show f) ++ show (uid f)) f


data Wiki = Wiki
    { listing :: Map FilePath File
    , places  :: Map Place (Maybe Pin)
    , eras    :: Map String Era
    }


new :: Wiki
new = Wiki Map.empty Map.empty Map.empty

record :: forall a. Note a => Wiki -> FilePath -> a -> Wiki
record w n f = w{ listing = Map.insert n (File f) (listing w) }

recordEra :: Wiki -> FilePath -> Era -> Wiki
recordEra w n e = w'{ eras = update (eras w') } where
    update dict = Map.unions [dict, afters dict, befores dict]
    afters = makeDict $ codes e
    befores = makeDict $ precodes e
    makeDict ks dict = foldr insert dict ks
    insert code = Map.insert code e
    w' = record w n e

recordCharacter :: Wiki -> FilePath -> Character -> Wiki
recordCharacter = record

recordPlace :: Wiki -> FilePath -> Place -> Wiki
recordPlace w n p = w'{ places = Map.insert p (parent p) (places w') }
    where w' = record w n p

-- | Internally resolve a map of i.e. Place -> Pin into a map of Place -> Place
remap :: (Internal i, Ord n, Note n) => Map n (Maybe Pin) -> i (Map n (Maybe n))
remap orig = Map.foldWithKey rebuild (pure Map.empty) orig where
    rebuild n mp idict = Map.insert n <$> thread (resolve orig) mp <*> idict
    -- find the note :: (Map n (Maybe Pin)) -> Pin -> i (Maybe n)
    resolve dict p = (findByUid . uid =<<) <$> find p where
        findByUid i = List.find ((== i) . uid) $ Map.keys dict

-- Directory building

-- | All the maps of pins on to files, in order of priority
maps :: Wiki -> [Map String [File]]
maps w = map (pinMap $ listing w) priorities where
    pinMap dir pri = Map.fold (addKeys pri) Map.empty dir
    addKeys pri file dict = foldr (addKey file) dict (namesFor pri file)
    namesFor pri = map slugify . ofPriority pri . names
    addKey file p = Map.insertWith (++) p [file]

-- | The list of all tags for all files (sorted)
tagList :: Wiki -> [(FilePath, String)]
tagList = convertToList . expandToTags . listing where
    convertToList = sort . concatMap expand . Map.toList
    expandToTags = Map.map (Set.toList . tags)
    expand (filename, ts) = zip (repeat filename) ts


-- | Error handling
-- | (And warning handling)

data Warning
    = NotFound Pin
    | Ambiguous Pin [File]
    | Unknown Pinpoint
    | NoSuchEra String
instance Show Warning where
    show (NotFound p) = printf "Can't find '%s'" $ show p
    show (Unknown p) = printf "Unknown pinpoint: '%s'" $ show p
    show (NoSuchEra e) = printf "No such era: %s" e
    show (Ambiguous p fs) = printf "Ambiguities looking for %s\n\t%s"
        (show p) (intercalate "\n\t" $ map show fs)
