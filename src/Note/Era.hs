module Note.Era where
import Control.Applicative hiding ( (<|>) )
import Control.DateTime.Moment ( Offset(..) )
import Control.Event ( Event )
import Control.Modifier
import Data.Body ( event )
import Internal
import Note ( Note(..), Basic, firstEvent )
import Text.ParserCombinators.Parsec ( GenParser )
import Text.Point
import qualified Control.Modifier as Mods
import qualified Data.Map as Map
import Data.Wiki

data Era = Era
    { base     :: Basic
    , codes    :: [String]
    , precodes :: [String]
    } deriving (Eq, Ord)

dawnPoint :: Point
dawnPoint = Point{side=Start, name="dawn"}

dawn :: Era -> Maybe Offset
-- dawn = event dawnPoint . body
-- TODO: not implemented
dawn = const $ Just Root

instance Note Era where
    basic = base
{-
    alter era@(Era _ x y) dir = dir{eras=updated} where
        updated = foldr modify (eras dir) (map pos x ++ map neg y)
        modify (d, key) = Map.insert key (d, era)
        pos = (,) Positive
        neg = (,) Negative

makeEra :: FilePath -> GenParser Char st Era
makeEra fp = do
    (n, ms) <- parseNote fp Mods.anyMod
    pure Era{ base = n
            , codes = prefixes ms
            , precodes = suffixes ms }

recordEra :: Directory -> Era -> Directory
recordEra dir era = dir{ eras = updateEras era (eras dir) } where
    updateEras = foldr insert . codes
    insert (key, direction) = Map.insert key (direction, era)

recordChar :: Directory -> Character -> Directory
recordChar dir _ = dir

recordPlace :: Directory -> Place -> Directory
recordPlace dir place = dir{ places = updatePlaces place (places dir) } where
    updatePlaces = case parent place of
        Nothing -> const
        Just p -> Map.insert (uid place) (

root :: (Internal i) => String -> i String
root "" = pure ""
root e = maybe (pure "") recurse =<< offset e where
    recurse (_, m) = safeRecurseEra (era m) root
            -}
