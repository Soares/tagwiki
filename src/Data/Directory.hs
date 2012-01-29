{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
module Data.Directory
    ( Directory(..)
    , File(..)
    , Momentable
    , checkForAmbiguities
    , maps -- TODO: remove (testing only)
    , offset
    , pinpoint
    , location
    , tags
    , allRefs
    ) where
import Prelude hiding ( log )
import Control.Applicative
import Control.Dangerous hiding ( Warning )
import Control.Dangerous.Extensions()
import {-# SOURCE #-} Control.DateTime.Moment
import Control.Monad.Reader
import Control.Monad.State
import Data.Function
import Data.List ( intercalate, sort )
import Data.Map ( Map )
import Data.Maybe
import Data.Utils
import Data.Record ( Record )
import Data.Tree ( Tree )
import Text.Printf
import Text.Reference
import Text.Render
import Data.Trail
import Data.Cache
import qualified Data.Body as Body
import qualified Data.Map as Map
import qualified Data.Record as Record
import Text.Pinpoint ( Pinpoint, pin, point, isSelf )

data File = forall a. Record a => File a
instance Show File where show (File f) = Record.name f
instance Eq File where (==) = (==) `on` Record.identifier
instance Record File where
    note (File r) = Record.note r
    dawn (File r) = Record.dawn r
    parent (File r) = Record.parent r
    alter (File r) = Record.alter r

class ( Applicative a
      , Errorable a
      , MonadReader Directory a
      , MonadState Trail a
      ) => Momentable a

-- Actual construction!
instance Momentable (StateT Trail (StateT Cache (ReaderT Directory Dangerous)))

data Directory = Dir { listing :: [File]
                     , eras    :: Map String (Direction, File)
                     , places  :: Maybe (Tree File) }



-- Directory building

-- Maps of strings on to files, in order of priority
maps :: Directory -> [Map Reference [File]]
maps dir = [build True files, build False files] where
    files = listing dir
    build :: Bool -> [File] -> Map Reference [File]
    build flag = foldr (addKeys flag) Map.empty
    addKeys :: Bool -> File -> Map Reference [File] -> Map Reference [File]
    addKeys flag file dict = foldr (addKey file) dict (Record.keys flag file)
    addKey :: File -> Reference -> Map Reference [File] -> Map Reference [File]
    addKey file r = Map.insertWith (++) r [file]

checkForAmbiguities :: (Applicative m, Errorable m, MonadReader Directory m) => m ()
checkForAmbiguities = mapM_ (warn . Ambiguous) =<< offenders where
    offenders = offendingLists . filterMaps <$> ask
    filterMaps :: Directory -> [Map Reference [File]]
    filterMaps = map (Map.filter conflicting) . maps
    offendingLists :: [Map Reference [File]] -> [[File]]
    offendingLists = concatMap Map.elems
    conflicting = (> 1) . length


tags :: Directory -> [(FilePath, String)]
tags = concatMap tagsForFile . listing where
    tagsForFile f = map ((,) $ Record.identifier f) (Record.tags f)

offset :: (Momentable m) => String -> m (Maybe (Direction, Moment))
offset str = era . eras =<< ask where
    era dict = maybe cantFind locate (Map.lookup str dict)
    failTuple (_, Nothing) = Nothing
    failTuple (x, Just y) = Just (x, y)
    liftSnd (x, y) = (,) x <$> y
    locate (side, file) = failTuple <$> liftSnd (side, Record.dawn file)
    cantFind = warn (NoSuchEra str) *> pure Nothing

pinpoint :: (Momentable m) => Pinpoint -> m Moment
pinpoint p = find present (getMoment . Record.contents) p where
    getMoment body = forceMaybe =<< Body.moment (point p) body
    forceMaybe Nothing = warn (Unknown p) *> pure present
    forceMaybe (Just x) = pure x

location :: (Momentable m) => Pinpoint -> m String
-- TODO: link needs its parameters switched
-- location p = find "" (pure . anchor) p where
location p = find "" (pure . anchor) p where
    anchor file = link (to file) (Record.name file)
    to file = href (show <$> point p) (Record.identifier file)

candidates :: (Momentable m) => Reference -> m [File]
candidates ref = headOr [] . stripNull . map candsFor <$> asks maps where
    candsFor = fromMaybe [] . Map.lookup ref
    stripNull = filter (not . null)

find :: (Momentable m) => a -> (File -> m a) -> Pinpoint -> m a
find x fn p | isSelf p = maybe (pure x) fn . currentFile =<< get
            | otherwise = candidates (fromPin $ pin p) >>= doFirst where
    doFirst [] = warn (NotFound p) *> pure x
    doFirst (file:xs) = do
        unless (null xs) (warn $ Ambiguous $ file : xs)
        trail <- get
        modify (descendRef p)
        modify (descendFile file)
        new <- get
        unless (verify new) (throw $ Cycle $ refTrail new)
        fn file <* put trail


data Warning = NotFound Pinpoint
             | Ambiguous [File]
             | Unknown Pinpoint
             | NoSuchEra String


data Error = NotYetImplemented | Cycle [Pinpoint]


instance Show Warning where
    show (NotFound p) = printf "Can't find '%s'" $ show p
    show (Unknown p) = printf "Unknown moment: '%s'" $ show p
    show (NoSuchEra e) = printf "No such era: %s" e
    show (Ambiguous fs) = printf "File names are ambiguous:\n\t%s"
        (intercalate "\n\t" $ map Record.identifier fs)


instance Show Error where
    show NotYetImplemented = "not yet implemented"
    show (Cycle fs) = printf "references form cycle:\n\t%s"
        (intercalate "\n\t" $ map show fs)



-- For debugging
allRefs :: Directory -> [Reference]
allRefs = sort . concatMap Map.keys . maps
