{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
module Data.Directory
    ( Directory(..)
    , State
    , Momentable
    , safeRecurseEra
    , files
    , offset
    , pinpoint
    , location
    , tags
    , allRefs
    ) where
import Control.Applicative
import Control.Dangerous hiding ( Warning )
import Control.Dangerous.Extensions()
import Control.Monad.Reader hiding ( guard )
import Control.Monad.State hiding ( State, guard )
import Data.Function
import Data.List ( intercalate, sort )
import Data.Map ( Map )
import Data.Maybe
import Data.State
import Data.Trail ( Trail )
import Data.Utils
import Prelude hiding ( log )
import Text.Pin ( Pin )
import Text.Pinpoint ( Pinpoint, pin, point, isSelf )
import Text.Printf
import Text.Reference
import Text.Render
import qualified Data.Body as Body
import qualified Data.Map as Map
import qualified Data.Record as Record
import {-# SOURCE #-} Control.DateTime.Moment
import Data.File

class ( MonadState State a ) => Stated a
class ( Applicative a
      , Errorable a
      , MonadReader Directory a
      , Stated a
      ) => Momentable a

-- Actual construction!
instance Momentable (StateT State (ReaderT Directory Dangerous))
instance (Monad m) => Stated (StateT State m)

data Directory = Dir { listing :: [File]
                     , eras    :: Map String (Direction, File)
                     , places  :: Map File Pin }


-- Directory building

-- Maps of strings on to files, in order of priority
maps :: Directory -> [Map Reference [File]]
maps dir = [build True lst, build False lst] where
    lst = listing dir
    build flag = foldr (addKeys flag) Map.empty
    addKeys flag file dict = foldr (addKey file) dict (Record.keys flag file)
    addKey file r = Map.insertWith (++) r [file]


tags :: Directory -> [(FilePath, String)]
tags = concatMap tagsForFile . listing where
    tagsForFile f = map ((,) $ Record.source f) (Record.tags f)

offset :: (Momentable m) => String -> m (Maybe (Direction, Moment))
offset str = cachedOffset str create where
    create = era . eras =<< ask
    era dict = maybe cantFind locate (Map.lookup str dict)
    failTuple (_, Nothing) = Nothing
    failTuple (x, Just y) = Just (x, y)
    liftSnd (x, y) = (,) x <$> y
    locate (side, file) = failTuple <$> liftSnd (side, Record.dawn file)
    cantFind = warn (NoSuchEra str) *> pure Nothing

safeRecurseEra :: (Momentable m) => String -> (String -> m String) -> m String
safeRecurseEra e rec = pushEra e *> guard Cycle *> rec e <* popEra

pinpoint :: (Momentable m) => Pinpoint -> m Moment
pinpoint p = cachedMoment p create where
    create = find present (bodyMoment . Record.contents) p
    bodyMoment body = forceMaybe =<< Body.moment (point p) body
    forceMaybe Nothing = warn (Unknown p) *> pure present
    forceMaybe (Just x) = pure x

location :: (Momentable m) => Pinpoint -> m String
location p = cachedLocation p create where
    create = find "" (pure . anchor) p
    anchor file = link (to file) (Record.name file)
    to file = href (show <$> point p) (Record.identifier file)

candidates :: (Momentable m) => Reference -> m [File]
candidates k = headOr [] . stripNull . map candsFor <$> asks maps where
    candsFor = fromMaybe [] . Map.lookup k
    stripNull = filter (not . null)

find :: (Momentable m) => a -> (File -> m a) -> Pinpoint -> m a
find x fn p | isSelf p = maybe (pure x) fn =<< getFile
            | otherwise = candidates (fromPin $ pin p) >>= doFirst where
    doFirst [] = warn (NotFound p) *> pure x
    doFirst (file:xs) = do
        unless (null xs) (warn $ Ambiguous $ file : xs)
        pushRef p file *> guard Cycle *> fn file <* popRef

files :: (Momentable m) => m [(FilePath, String)]
files = ask >>= sequence . filePairs where
    idxFiles dir = zip (listing dir) [0..]
    filePairs = map makePair . idxFiles
    makePair (f, i) = fstAndLiftedSnd (Record.filename i) Record.text f
    fstAndLiftedSnd :: (Functor f) => (z -> a) -> (z -> f b) -> z -> f (a, b)
    fstAndLiftedSnd x y z = (,) (x z) <$> y z

data Warning = NotFound Pinpoint
             | Ambiguous [File]
             | Unknown Pinpoint
             | NoSuchEra String


data Error = NotYetImplemented | Cycle Trail


instance Show Warning where
    show (NotFound p) = printf "Can't find '%s'" $ show p
    show (Unknown p) = printf "Unknown moment: '%s'" $ show p
    show (NoSuchEra e) = printf "No such era: %s" e
    show (Ambiguous fs) = printf "File names are ambiguous:\n\t%s"
        (intercalate "\n\t" $ map Record.identifier fs)


instance Show Error where
    show NotYetImplemented = "not yet implemented"
    show (Cycle trl) = printf "references form cycle:\n\t%s" (show trl)



-- For debugging
allRefs :: Directory -> [Reference]
allRefs = sort . concatMap Map.keys . maps
