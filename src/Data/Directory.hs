module Data.Directory
    ( Directory(Directory)
    , Operation
    , Key(..)
    , eraOffset
    , eraOffsets
    , pinpoint
    , location
    , taglist
    , files
    ) where
import Control.Applicative
import Control.Monad.Reader
import Control.Dangerous hiding ( Warning )
import Control.DateTime.Moment
import Control.DateTime.Offset
import Data.Maybe
import Data.List ( intercalate )
import Text.Render
import Text.Printf
import Text.Fragment
import Data.Body ( Body, moment )
import Control.Reference ( Reference(events) )

data Key = Key { ident      :: String
               , name       :: String
               , pseudonyms :: [String]
               , categories :: [String]
               , tags       :: [String]
               , matches    :: Reference -> Operation (Maybe Bool)
               , offset     :: String -> Maybe Offset
               , within     :: Maybe String
               }

data Directory = Directory { listing :: [(Key, Body)] }
type Handle = (Key, Body)
type Operation = DangerousT (Reader Directory)

taglist :: Directory -> [String]
taglist = concatMap (tags . fst) . listing

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

handle :: a -> (Handle -> Operation a) -> Reference -> Operation a
handle def fn ref = handle' =<< found where
    handle' Nothing = pure def
    handle' (Just h) = fn h
    handles :: Operation [Handle]
    handles = listing <$> lift ask
    check :: Handle -> Operation (Maybe Bool, Handle)
    check h@(k, _) = (,) <$> matches k ref <*> pure h
    matchings :: Operation [(Maybe Bool, Handle)]
    matchings = handles >>= mapM check
    candidates :: Operation [(Maybe Bool, Handle)]
    candidates = filter (isJust . fst) <$> matchings
    pris :: Operation [Handle]
    pris = (\ms -> [t | (Just True, t) <- ms]) <$> candidates
    commons :: Operation [Handle]
    commons = (\ms -> [t | (Just False, t) <- ms]) <$> candidates
    found :: Operation (Maybe Handle)
    found = join $ find <$> pris <*> commons
    find :: [Handle] -> [Handle] -> Operation (Maybe Handle)
    find xs ys = case (length xs, length ys) of
        (0, 0) -> warn (NotFound ref) *> pure Nothing
        (0, 1) -> pure $ Just $ head ys
        (1, _) -> pure $ Just $ head xs
        (_, _) -> warn (TooMany ref z zs) *> pure (Just z)
            where (z:zs) = if null xs then ys else xs

maybeFirst :: [a] -> Maybe a
maybeFirst [] = Nothing
maybeFirst (x:_) = Just x

firstOr :: a -> [a] -> a
firstOr x [] = x
firstOr _ (x:_) = x

data Warning = NotFound Reference | TooMany Reference Handle [Handle]
instance Show Warning where
    show (NotFound ref) = printf
        "Can't find reference: %s" $ show ref
    show (TooMany ref x xs) = printf
        "Found multiple matches for %s. Using %s, Ignoring:\n\t%s"
            (show ref)
            (ident $ fst x)
            (intercalate "\n\t" $ map (ident. fst) xs)

data Error = NotYetImplemented deriving Show
