{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
module Data.Directory
    ( Directory(..)
    , File(..)
    , Momentable
    , Operable
    , checkForAmbiguities
    , offset
    , pinpoint
    , location
    , tags
    ) where
import Prelude hiding ( log )
import Control.Applicative
import Control.Dangerous hiding ( Warning )
import Control.Dangerous.Extensions()
import {-# SOURCE #-} Control.DateTime.Moment
import Control.Monad.Reader
import Control.Monad.State
import Data.Function
import Data.List ( intercalate )
import Data.Map ( Map )
import Data.Maybe
import Data.Record ( Record )
import Data.Tree ( Tree )
import Data.Utils
import Text.Pin ( Pin )
import Text.Point ( Point )
import Text.Printf
import Text.Reference
import Text.Render
import qualified Data.Body as Body
import qualified Data.Map as Map
import qualified Data.Record as Record
import qualified Text.Pin as Pin
import qualified Text.Point as Point

data File = forall a. Record a => File a
instance Eq File where (==) = (==) `on` Record.identifier
instance Record File where
    note (File r) = Record.note r
    dawn (File r) = Record.dawn r
    parent (File r) = Record.parent r

class ( Applicative a
      , Errorable a
      , MonadReader Directory a
      ) => Operable a

class ( Operable a
      , MonadState [String] a
      ) => Momentable a

class ( Operable a
      , MonadState [File] a
      ) => Restricted a

instance (Operable a) => Operable (StateT [File] a)
instance (Operable a) => Operable (StateT [String] a)
instance (Operable a) => Restricted (StateT [File] a)

-- Actual construction!
instance Operable (ReaderT Directory Dangerous)
instance Momentable (StateT [String] (ReaderT Directory Dangerous))

data Directory = Dir { listing :: [File]
                     , eras    :: Map String File
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
offset str = ask >>= era . eras where
    era dict = pure $ (Record.dawn =<<) $ Map.lookup str dict

pinpoint :: (Momentable m) => Pin -> Maybe Point -> m Moment
pinpoint pin point = operate present (getMoment . Record.contents) pin where
    getMoment body = Body.moment point body >>= forceMoment
    forceMoment m = fromMaybe <$> usePresent <*> pure m
    usePresent = warn (Unknown pin point) *> pure present

location :: (Operable m) => Pin -> Maybe Point -> m String
-- TODO: link needs its parameters switched
location pin point = operate "" (pure . anchor) pin where
    anchor file = link (to file) (Record.name file)
    to file = href (Point.name <$> point) (Record.identifier file)

descend :: (Restricted m) => File -> m File
descend file = (get >>= check) *> descend' *> pure file where
    descend' = modify (file:)
    check files = when (file `elem` files) (err files)
    -- TODO: Add the era to the front of the era cycle, like this:
    err files = throw $ Cycle (file:files)

find :: (Restricted m) => Pin -> m (Maybe File)
find pin = if pin == Pin.empty then current else matches >>= getFirst where
    current = maybeHead <$> get
    -- matches :: (Operable m) => m [File]
    matches = matches' <$> asks maps where
        matches' :: [Map Reference [File]] -> [File]
        matches' = concat . matchLists
        matchLists :: [Map Reference [File]] -> [[File]]
        matchLists = map getMatches
        getMatches :: Map Reference [File] -> [File]
        getMatches = fromMaybe [] . Map.lookup ref
        ref = fromPin pin
    -- getFirst :: (Operable m) => [File] -> m (Maybe File)
    getFirst [] = warn (NotFound pin) *> pure Nothing
    getFirst (x:_) = Just <$> descend x

operate :: (Operable m) => a -> (File -> m a) -> Pin -> m a
operate x fn pin = maybe (pure x) fn =<< fst <$> runStateT (find pin) []

-- operate x fn pin = maybe (pure x) (fn . fst) (runStateT (find pin) [])


data Warning = NotFound Pin | Ambiguous [File] | Unknown Pin (Maybe Point)


data Error = NotYetImplemented | Cycle [File]


instance Show Warning where
    show (NotFound pin) = printf "Can't find '%s'" $ show pin
    show (Unknown pin mp) = printf "Can't find moment for '%s%s'"
        (show pin) (maybe "" (('#':) . show) mp)
    show (Ambiguous fs) = printf "File names are ambiguous:\n%s"
        (intercalate "\n\t" $ map Record.identifier fs)


instance Show Error where
    show NotYetImplemented = "not yet implemented"
    show (Cycle fs) = printf "references form cycle:\n%s"
        (intercalate "\n\t" $ map Record.identifier fs)
