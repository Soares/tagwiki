module Data.Era where
import Data.Directory
import Control.Applicative hiding ( (<|>) )
import Data.Note ( Note, firstEvent, parseNote )
import qualified Data.Map as Map
import Text.Point
import Control.Modifier
import Control.DateTime.Moment
import qualified Control.Modifier as Mods
import Text.ParserCombinators.TagWiki
import Data.Record hiding ( name )

data Era = Era { base     :: Note
               , codes    :: [String]
               , precodes :: [String]
               } deriving (Eq, Ord)

dawnPoint :: Point
dawnPoint = Point{side=Start, name="dawn"}

instance Record Era where
    note = base
    dawn = firstEvent (Just dawnPoint) . note
    alter era@(Era _ x y) dir = dir{eras=updated} where
        updated = foldr modify (eras dir) (map pos x ++ map neg y)
        modify (d, key) = Map.insert key (d, File era)
        pos = (,) Positive
        neg = (,) Negative

instance Parseable Era where
    parser = do
        (n, ms) <- parseNote Mods.anyMod
        pure Era{ base = n
                , codes = prefixes ms
                , precodes = suffixes ms }
