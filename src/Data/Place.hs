module Data.Place where
import Control.Appearance
import Control.Applicative hiding ( (<|>) )
import Control.Modifier
import Data.Directory
import Data.File
import Data.Note ( Note, firstAppearance, parseNote )
import Data.Record hiding ( name, pin, ref )
import Text.ParserCombinators.Parsec ( GenParser )
import Text.Pinpoint
import qualified Control.Modifier as Mods
import qualified Data.Map as Map

-- TODO: handle suffixes (auto-tag)
-- TODO: handle prefixes (sizing)
-- TODO: parent is in qualifiers
newtype Place = Place { base :: Note } deriving (Eq, Ord)

instance Record Place where
    note = base
    alter p dir = case parent p of
        Nothing -> dir
        Just r -> dir{places=updated $ places dir} where
            updated = Map.insert (File p) r
    parent = (pin . ref <$>) . firstAppearance . note

makePlace :: FilePath -> GenParser Char st Place
makePlace fp = do
    (n, _) <- parseNote fp $ Mods.parse [category, qualifier, suffix]
    pure Place{ base = n }
