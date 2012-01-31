module Data.Body where
import Control.Appearance ( Appearance )
import Control.Applicative hiding ( (<|>), empty )
import Control.Attribute ( Attribute )
import Control.Event ( Event, recognizes )
import Control.Unit ( section, Unit )
import Data.List ( intercalate )
import Data.Utils
import Text.Fragment
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.TagWiki
import Text.Point ( Point(name) )
import Text.Printf
import qualified Text.Render as Render



data Body = Body
    { attrs  :: [Attribute]
    , events :: [Event]
    , apps   :: [Appearance]
    , units  :: [Unit]
    } deriving Eq

instance Fragment Body where
    resolve (Body at ev ap un) = intercalate "\n\n" <$> sections where
        sections = sequence [atts, evts, aps, unts]
        join = intercalate "\n"
        atts = (Render.section "Attributes" . join) <$> mapM resolve at
        evts = (Render.section "Events" . join) <$> mapM resolve ev
        aps = (Render.section "Appearances" . join) <$> mapM resolve ap
        unts = (Render.section "Notes" . concat) <$> mapM resolve un


-- Event finding
event :: Maybe Point -> Body -> Maybe Event
event pt = maybeHead . filter (recognizes p) . events
    where p = maybe "" name pt

-- Parsing
empty :: Body
empty = Body [] [] [] []

instance Parseable Body where
    parser = foldr id empty <$> fill `manyTill` eof

fill :: GenParser Char st (Body -> Body)
fill = try (addSec <$> section)
   <|> try (addAttr <$> parser)
   <|> try (addEvent <$> parser)
   <|> (addApp <$> parser) where
    addSec   x r = r{units=x ++ units r}
    addAttr  x r = r{attrs=x:attrs r}
    addEvent x r = r{events=x:events r}
    addApp   x r = r{apps=x:apps r}



instance Show Body where
    show (Body as es ps us) = printf "%s\n%s\n%s\n%s"
        (intercalate "\n" $ map show as)
        (intercalate "\n" $ map show es)
        (intercalate "\n" $ map show ps)
        (concatMap show us)
