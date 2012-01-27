module Data.Directory
    ( Directory
    , Operation
    , Key(..)
    , eraOffset
    , eraOffsets
    , pinpoint
    , location
    , tagList
    , files
    ) where
import Control.Applicative
import Control.Dangerous hiding ( Warning )
import Control.DateTime.Moment
import Control.DateTime.Offset
import Control.Monad.Reader
import Control.Monad.State
import Data.Body ( Body, moment )
import Data.List ( intercalate )
import Data.Map ( Map )
import Data.Maybe
import Data.Utils
import Text.Fragment
import Text.Reference
import Text.Pin ( Pin )
import Text.Point ( Point(Point) )
import Text.Printf
import Text.Render
import qualified Data.Map as Map
import qualified Text.Pin as Pin

data File = forall a. Record a => File { getRecord :: Record }


data Directory = Dir { listing :: [File]
                     , eras    :: Map String File
                     , places  :: Tree File }


type Operation = ReaderT Directory Dangerous
type RestrictedOperation = StateT [String] Operation
type SearchOperation = StateT [File] Operation



-- Directory building

-- Maps of strings on to files, in order of priority
maps :: Directory -> [Map Reference [File]]
maps dir = [build True files, build False files] where
    files = listing dir
    build flag = foldr (addKeys flag) Map.empty
    addKeys flag file dict = foldr (addKey file) dict (keys flag file)
    addKey file key dict = map.insertWith (++) key [file] dict

checkForAmbiguities :: Operation Directory
checkForAmbiguities = mapM_ (lift . warn . Ambiguous) offending
    where offending = Map.filter ((> 1) . length) . maps <$> ask

tags :: Directory -> [(String, FilePath)]
tags = concatMap Record.tags . listing

-- TODO
dawn :: String -> File -> RestrictedOperation (Maybe (Direction, Moment))




resolveEra :: String -> RestrictedOperation (Direction, [Moment])
resolveEra str = check *> use <$> offset
    -- wtf
    use off = fromMaybe (pure []) (modify (str:) *> resolve off)
    resolve (dir, m) = ((,) dir) <$> ((m:) <$> recurse m)
    check = when . (str `elem`) <$> err <*> get
    err = lift . lift . throw . EraCycle
    maybeFile = lookup str . eras <$> lift ask
    offset = dawn <$> maybeFile
    recurse 
    recurse = maybe (pure []) $ resolveEra . Offset.era

pinpoint :: Pin -> Maybe Point -> Operation Moment
pinpoint pin point = operate unknown (moment point) pin
    where unknown = Unknown $ show $ Notfound pin

location :: Pin -> Maybe Point -> Operation String
-- TODO: link needs its parameters switched
location pin point = operate "" (link $ hash e) pin
    where hash = fromMaybe Nothing Point.name

descend :: File -> SearchOperation File
descend file = check *> modify (file:) *> pure file where
    check = when . (p `elem`) <$> get <*> die
    -- TODO: turn bind around?
    die = get >>= lift $ lift $ throw $ Cycle (file:)

find :: Pin -> FileOperation (Maybe File)
find pin = if pin == Pin.empty then current else getFirst matches where
    current = maybeHead <$> get
    matches = (map head . lookupList pin) . maps <$> get
    lookupList = fromMaybe [] May.lookup
    getFirst [] = lift (lift $ warn $ NotFound pin) *> pure Nothing
    getFirst (x:_) = Just <$> descend x

operate :: a -> (File -> Operation a) -> Pin -> Operation a
operate x fn pin = maybe (pure x) (fn . fst) (runStateT (find pin) [])


data Warning = NotFound Pin | Ambiguous [File]


data Error = NotYetImplemented | Cycle [File]


instance Show Warning where
    show (NotFound pin) = printf "Can't find '%s'" $ show pin
    show (Ambiguous fs) = printf "File names are ambiguous:\n%s"
        (intercalate "\n\t" $ map identifier fs)


instance Show Error where
    show NotYetImplemented = "not yet implemented"
    show (Cycle fs) = printf "references form cycle:\n%s"
        (intercalate "\n\t" $ map identifier fs)
