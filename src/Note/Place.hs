{-# LANGUAGE MultiParamTypeClasses #-}
module Note.Place where
import Control.Appearance
import Control.Applicative hiding ( (<|>) )
import Control.Name
import Data.Body
import Data.Function
import Data.String.Utils ( strip )
import Data.Utils
import Location
import Note
import Text.ParserCombinators.Parsec ( GenParser )
import Text.Pin ( Pin )
import Text.Pinpoint ( pin )
import Text.Printf
import Text.Utils
import qualified Control.Modifier as Mods
import qualified Data.Set as Set

newtype Place = Place { base :: Basic } deriving Eq

everywhere :: Place
everywhere = Place Note.empty

-- TODO
size :: Place -> Rational
size = const 1

-- TODO
timezone :: Place -> Int
timezone = const 1

parent :: Place -> Maybe Pin
parent = fmap (pin . ref) . maybeHead . apps . body

instance Ord Place where (<=) = (<=) `on` uid
instance Show Place where show = primaryName
instance Bubble Place where
    label = primaryName
    amount = size

instance Note Place where
    basic = base

-- | The name of the empty place is «everywhere»
    primaryName p | null $ names $ basic p = "«everywhere»"
                  | otherwise = primaryName $ basic p

-- | If you have the following place:
-- |
-- |    Central
-- |    $Providence
-- |
-- | then the following names will all be recognizable:
-- |
-- |    Central
-- |    Providence of Central
-- |    Central Providence
    names p = names (basic p) ++ newNames (suffixes p) (names $ basic p) where
        newNames = concatMap . placeName
        placeName = inflate . inflater
        inflater ss n = concatMap (addSuffix n) ss
        addSuffix n s = [strip s ++ " of " ++ n, n ++ " " ++ strip s]

-- | Place parents are included in the first tag of each place.
-- | The first place suffix is included ('of' format) in the first tag.
-- | If you have the following two places:
-- |
-- |    Midland
-- |    $Providence
-- |    @Atrea
-- |
-- |    Midland
-- |    $Providence
-- |    @Malloria
-- |
-- | Then they will be tagged thusly:
-- |
-- |    Providence of Midland (Atrea)
-- |    Providence of Midland (Malloria)
    tags p = Set.fromList . alter . map namePart . names $ basic p where
        alter [] = []
        alter (t:ts) = (pre ++ t ++ suf):ts
        pre = doHead "" (++ " of ") (suffixes p)
        suf = maybe "" (printf " (%s)" . show) (parent p)

-- | Suffixes are included in categories. So the following place
-- |
-- | Whiteshore
-- | #Capitol $City
-- | @Malloria
-- |
-- | Will have the categories #capitol and #city
    categories p = categories (basic p) `Set.union` extras (suffixes p) where
        extras = Set.fromList . map slugify

-- | For purposes of looking up places in links, the parent will be
-- | included as a qualifier. Thus, with the above places as examples,
-- | the links |Midland (Atrea)| and |Midland (Malloria)| are valid.
    qualifiers p = qualifiers (basic p) `Set.union` extras where
        extras = Set.fromList $ maybe [] pure (parent p)


parsePlace :: Int -> GenParser Char st Place
parsePlace i = Place <$> parseBasic i (Mods.parse mods) where
    mods = [Mods.category, Mods.qualifier, Mods.suffix]
