module Data.Directory
    ( Directory(Directory)
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
import Control.Monad.Reader
import Control.Dangerous hiding ( Warning )
import Control.DateTime.Moment
import Control.DateTime.Offset
import Data.Maybe
import Data.Map ( Map )
import qualified Data.Map as Map
import Data.List ( intercalate )
import Text.Render
import Text.Printf
import Text.FuzzyString
import Text.Fragment
import Data.Body ( Body, moment )
import Control.Reference ( Reference(events) )

data Key = Key { ident      :: String
               , name       :: String
               , pseudonyms :: [String]
               , categories :: [String]
               , qualifiers :: [String]
               , tags       :: [String]
               , matches    :: [(FuzzyString, Bool)]
               , offset     :: String -> Maybe Offset
               , within     :: Maybe String
               }

data Directory = Directory { listing :: [(Key, Body)] }
type Operation = DangerousT (Reader Directory)

tagList :: Directory -> [String]
tagList = concatMap (tags . fst) . listing

eraOffset :: String -> Operation (Maybe Offset)
eraOffset str = maybeFirst . mapMaybe getOffset . listing <$> lift ask
    where getOffset (x, _) = offset x str

eraOffsets :: String -> Operation [Offset]
eraOffsets str = chain =<< eraOffset str where
    chain (Just o) = (o:) <$> eraOffsets (era o)
    chain Nothing = pure []

pinpoint :: Reference -> Operation Moment
pinpoint ref = handle unknown pinpoint' ref where
    pinpoint' (_, b) = moment (events ref) b
    unknown = Unknown $ show $ NotFound ref

location :: Reference -> Operation String
location ref = handle "" (pure . link') ref where
    link' (k, _) = href (ident k) (firstOr "" (events ref))

files :: Operation [(String, String)]
files = mapM file . listing =<< lift ask where
    file (k, b) = (,) (name k) <$> contents (k, b)

contents :: (Key, Body) -> Operation String
contents (k, b) = ((top ++ "\n") ++) <$> bottom where
    top = concat [tit, hed, aka, cats, toc]
    tit = title (name k)
    hed = header (name k)
    aka = section "Pseudonyms" (list $ pseudonyms k)
    cats = section "Categories" (list $ categories k)
    toc = reference
        [ ("attributes", "Attributes")
        , ("appearances", "Appearances")
        , ("events", "Events")
        , ("notes", "Notes") ]
    bottom = resolve b


priorityMap :: Directory -> Map FuzzyString [(Key, Body)]
priorityMap = buildMap True . listing

commonMap :: Directory -> Map FuzzyString [(Key, Body)]
commonMap = buildMap False . listing

buildMap :: Bool -> [(Key, Body)] -> Map FuzzyString [(Key, Body)]
buildMap b = foldr insertAll Map.empty where
    insert str kb = Map.insertWith (++) str [kb]
    insertAll kb@(k, _) tree = foldr (insertOne kb) tree (matches k)
    insertOne kb (str, pri) tree | pri == b = insert str kb tree
                                 | otherwise = tree

find :: Reference -> Operation (Maybe (Key, Body))
find ref = do
    warn (Looking ref)
    let get = fromMaybe [] . Map.lookup (fromRef ref)
    xs <- get <$> lift (asks priorityMap)
    warn (Testing "wtfpri" xs)
    ys <- get <$> lift (asks commonMap)
    warn (Testing "wtfcom" ys)
    case (length xs, length ys) of
        (0, 0) -> warn (NotFound ref) *> pure Nothing
        (0, 1) -> pure (Just $ head ys)
        (1, _) -> pure (Just $ head xs)
        (_, _) -> warn (TooMany ref z zs) *> pure (Just z)
            where (z:zs) = if null xs then ys else xs

handle :: a -> ((Key, Body) -> Operation a) -> Reference -> Operation a
handle def fn ref = handle' =<< find ref where
    handle' Nothing = pure def
    handle' (Just h) = fn h

maybeFirst :: [a] -> Maybe a
maybeFirst [] = Nothing
maybeFirst (x:_) = Just x

firstOr :: a -> [a] -> a
firstOr x [] = x
firstOr _ (x:_) = x

data Warning = NotFound Reference
             | TooMany Reference (Key, Body) [(Key, Body)]
             | Testing String [(Key, Body)]
             | Looking Reference
instance Show Warning where
    show (NotFound ref) = printf
        "Can't find reference: %s" $ show ref
    show (TooMany ref x xs) = printf
        "Found multiple matches for %s. Using %s, Ignoring:\n\t%s"
            (show ref)
            (ident $ fst x)
            (intercalate "\n\t" $ map (ident. fst) xs)
    show (Testing str xs) = printf "Note(%s): <%s>" str
            (intercalate "|" $ map (ident. fst) xs)
    show (Looking ref) = printf
        "Looking for: %s" $ show ref

data Error = NotYetImplemented deriving Show
