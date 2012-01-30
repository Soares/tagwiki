module Data.Place where
import Control.Appearance
import Control.Applicative hiding ( (<|>) )
import Control.Modifier ( anyMod, prefixes, suffixes )
import Data.Directory hiding ( tags )
import Data.File
import Data.Note ( Note(..), firstAppearance, parseNote )
import Data.String.Utils ( strip )
import Data.Utils
import Data.Record ( Record(note, alter, parent) )
import Text.ParserCombinators.Parsec ( GenParser, parse )
import Text.ParserCombinators.Parsec.Error ( errorMessages, messageString )
import Text.ParserCombinators.TagWiki
import Text.Pinpoint
import Text.Printf
import Text.Utils
import qualified Data.Map as Map

data Place = Place
    { base  :: Note
    , size  :: Double
    } deriving (Eq, Ord)

instance Record Place where
    note = base
    alter p dir = case parent p of
        Nothing -> dir
        Just r -> dir{places=updated $ places dir} where
            updated = Map.insert (File p) r
    parent = (pin . ref <$>) . firstAppearance . note

makePlace :: FilePath -> GenParser Char st Place
makePlace fp = do
    (n, ms) <- parseNote fp anyMod
    let ps = prefixes ms
    let ss = suffixes ms
    case parse real fp (headOr "1" ps) of
        Right dub -> pure Place{base = updateNote ss n, size=dub}
        Left err -> fail $ errmsg err where
            errmsg = unlines . map messageString . errorMessages

updateNote :: [String] -> Note -> Note
updateNote ss n = n{ names = ns ++ placeNames ss ns
                   , tags = placeTags n ss ts
                   , categories = cs ++ placeCategories ss
                   , qualifiers = qs ++ placeQualifiers n
                   } where
    cs = categories n
    qs = qualifiers n
    ns = names n
    ts = tags n


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
placeNames :: [String] -> [(Bool, String)] -> [(Bool, String)]
placeNames = concatMap . placeName where
    placeName ss (p, n) = zip (repeat p) (concatMap (addSuffix n) ss)
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
placeTags :: Note -> [String] -> [String] -> [String]
placeTags _ _ [] = []
placeTags n ss (t:ts) = upgraded:ts where
    upgraded = pre ++ t ++ suf
    pre = if null ss then "" else head ss ++ " of "
    suf = case firstAppearance n of
            Nothing -> ""
            Just a -> printf " (%s)" (show $ ref a)


-- | For purposes of looking up places in links, the parent will be
-- | included as a qualifier. Thus, with the above places as examples,
-- | the links |Midland (Atrea)| and |Midland (Malloria)| are valid.
placeQualifiers :: Note -> [Pinpoint]
placeQualifiers n = case firstAppearance n of
                        Nothing -> []
                        Just a -> [ref a]


-- | Suffixes are included in categories. So the following place
-- |
-- | Whiteshore
-- | #Capitol $City
-- | @Malloria
-- |
-- | Will have the categories #capitol and #city
placeCategories :: [String] -> [String]
placeCategories = map slugify
