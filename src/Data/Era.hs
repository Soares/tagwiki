module Data.Era where
import Control.Applicative hiding ( (<|>) )
import Data.Note ( Note, firstEvent, parseNote )
import Text.Point
import Control.Modifier
import qualified Control.Modifier as Mods
import Text.ParserCombinators.TagWiki
import Text.ParserCombinators.Parsec
import Data.Record hiding ( name )

data Era = Era { base     :: Note
               , codes    :: [String]
               , precodes :: [String] }
               deriving Eq

dawnPoint :: Point
dawnPoint = Point{side=Start, name="dawn"}

instance Record Era where
    note = base
    dawn = firstEvent (Just dawnPoint) . note

instance Parseable Era where
    parser = do
        (n, ms) <- parseNote eraMods
        pure Era{ base = n
                , codes = prefixes ms
                , precodes = suffixes ms }

eraMods :: GenParser Char st Modifier
eraMods = Mods.parse [category, qualifier, prefix, suffix]
