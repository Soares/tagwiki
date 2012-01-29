module Data.Place where
import Data.Directory
import Control.Appearance
import Text.Pinpoint
import Control.Applicative hiding ( (<|>) )
import Data.Note ( Note, firstAppearance, parseNote )
import qualified Data.Map as Map
import Text.ParserCombinators.TagWiki
import Control.Modifier
import qualified Control.Modifier as Mods
import Data.Record hiding ( name, pin )

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

instance Parseable Place where
    parser = do
        (n, _) <- parseNote $ Mods.parse [category, qualifier, suffix]
        pure Place{ base = n }
