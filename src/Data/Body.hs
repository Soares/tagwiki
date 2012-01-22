module Data.Body where
import Control.Appearance ( Appearance )
import Control.Applicative ( (<$>) )
import Control.Attribute ( Attribute )
import Control.Event ( Event )
import Control.Unit ( section, Unit )
import Data.List ( intercalate )
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.TagWiki
import Text.Printf



data Body = Body { attrs  :: [Attribute]
                 , events :: [Event]
                 , apps   :: [Appearance]
                 , units  :: [Unit]
                 } deriving Eq




empty :: Body
empty = Body [] [] [] []




-- Parsing
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
