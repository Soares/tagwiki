module Data.Note where
import Control.Appearance ( Appearance )
import Data.Utils
import Control.Arrow
import Control.Applicative hiding ( (<|>) )
import Data.Set ( fromList )
import Control.DateTime.Moment
import Data.Body ( Body )
import qualified Data.Body as Body
import Data.List
import Data.Record ( Record )
import Text.Point ( Point )
import Text.Tag ( tag )
import Control.Modifier ( Modifier(..) )
import qualified Control.Modifier as Mods
import Text.ParserCombinators.TagWiki
import Text.ParserCombinators.Parsec
import Text.Printf
import qualified Text.Symbols as Y
import Text.Pin ( Pin(Pin) )
import qualified Data.Record as Record
import {-# SOURCE #-} Data.Directory ( Momentable )

data Note = Note { names      :: [(Bool, String)]
                 , tags       :: [String]
                 , categories :: [String]
                 , qualifiers :: [String]
                 , body       :: Body
                 }

pin :: String -> Note -> Pin
pin s n = Pin s (categories n) (qualifiers n)

instance Record Note where
    note = id

instance Eq Note where
    x == y = ns && qs where
        ns = fromList (names x) == fromList (names y)
        qs = fromList (qualifiers x) == fromList (qualifiers y)

instance Ord Note where
    x <= y = pair x <= pair y where
        pair = fromList . names &&& fromList . qualifiers

instance Parseable Note where
    parser = fst <$> parseNote Mods.catOrQual

firstEvent :: (Momentable m) => Maybe Point -> Note -> m (Maybe Moment)
firstEvent pt = Body.moment pt . body

firstAppearance :: Note -> Maybe Appearance
firstAppearance = maybeHead . Body.apps . body

parseNote :: GenParser Char st Modifier -> GenParser Char st (Note, [Modifier])
parseNote parseMods = do
    ns <- firstLine
    mods <- parseMods `manyTill` (whitespace *> eol)
    let qs = Mods.qualifiers mods
    let cs = Mods.categories mods
    b <- parser
    pure (Note{ names = ns
              , tags = map (makeTag qs . snd) ns
              , categories = cs
              , qualifiers = qs
              , body = b }, mods)

makeTag :: [String] -> String -> String
makeTag qs n = unwords (n:map showq (sort qs)) where
    showq = printf "(%s)"

firstLine :: GenParser Char st [(Bool, String)]
firstLine = (name `sepBy` designator Y.comma) <* eol where
    name= whitespace >> (,) <$> priority <*> str where
        priority = option False (pri >> return True)
        escPri = hack >> pri
        str = (++) <$> option "" escPri <*> tag
        pri = string Y.priority
