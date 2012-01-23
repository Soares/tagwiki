module Data.Directory
    ( Directory(Directory)
    , Operation
    , Key(..)
    , eraOffset
    , eraOffsets
    , pinpoint
    , location
    , taglist
    ) where
import Control.Applicative
import Control.Monad.Reader
import Control.Dangerous hiding ( Warning )
import Control.DateTime.Moment
import Control.DateTime.Offset
import Data.Maybe
import Text.Render
import Text.Printf
import Data.Body
import Control.Reference

data Key = Key { ident   :: String
               , name    :: String
               , tags    :: [String]
               , matches :: Reference -> Maybe [String]
               , offset  :: String -> Maybe Offset
               , within  :: Maybe String
               }

data Directory = Directory { listing :: [(Key, Body)] }
type Handle = (Key, Body, [String])
type Operation = DangerousT (Reader Directory)

taglist :: Directory -> [String]
taglist = concatMap (tags . fst) . listing

eraOffset :: String -> Operation (Maybe Offset)
eraOffset str = first . mapMaybe getOffset . listing <$> lift ask
    where getOffset (x, _) = offset x str

eraOffsets :: String -> Operation [Offset]
eraOffsets str = chain =<< eraOffset str where
    chain (Just o) = (o:) <$> eraOffsets (era o)
    chain Nothing = return []

--eraOffsets str = offsets' <*> (eraOffset str) where
    --offsets' Nothing = return []
    --offsets' (Just o) = (o:) <$> eraOffsets (era o)

pinpoint :: Reference -> Operation Moment
pinpoint ref = handle unknown pp' ref where
    pp' _ = throw NotYetImplemented
    unknown = Unknown $ show $ NotFound ref

location :: Reference -> Operation String
location = handle "" (pure . link') where link' (k, _, es) = href (ident k) es

handle :: a -> (Handle -> Operation a) -> Reference -> Operation a
handle def fn ref = handle' =<< found where
    handle' Nothing = warn (NotFound ref) *> pure def
    handle' (Just h) = fn h
    found = (first . mapMaybe getMatch . listing) <$> lift ask
    getMatch (x, y) = (,,) x y <$> matches x ref

first :: [a] -> Maybe a
first [] = Nothing
first (x:_) = Just x

data Warning = NotFound Reference
instance Show Warning where
    show (NotFound ref) = printf "Can't find reference: %s" $ show ref

data Error = NotYetImplemented deriving Show
