module Text.TagWiki.Record where
import Control.Applicative ( (<*) )
import Control.Monad
import Data.Functor
import Text.Parser
import Text.ParserCombinators.Parsec
import Text.TagWiki.Appearance
import Text.TagWiki.Attribute
import Text.TagWiki.Event
import Text.TagWiki.Modifier
import Text.TagWiki.Reference ( Category, Qualifier )
import Text.TagWiki.Tag ( tag )
import Text.TagWiki.Unit
import qualified Text.TagWiki.Symbols as Y

-- Unit data types
data FileType = Character | Place | Note deriving (Eq, Read, Show)

data Name = Name Bool String deriving (Eq, Ord, Read, Show)
instance Parseable Name where
    parser = whitespace >> liftM2 Name priority str where
        priority = option False (pri >> return True)
        escPri = hack >> pri
        str = liftM2 (++) (option "" escPri) (tag <$> parser)
        pri = string Y.priority

-- A whole file
data Record = Record { names       :: [Name]
                     , categories  :: [Category]
                     , qualifiers  :: [Qualifier]
                     , modifiers   :: [Modifier]
                     , attrs       :: [Attribute]
                     , events      :: [Event]
                     , appearances :: [Appearance]
                     , units       :: [Unit]
                     } deriving (Eq, Show)
instance Parseable Record where
    parser = do
        tags <- firstLine
        (cats, quals, mods) <- secondLine
        fillers <- fill `manyTill` eof
        let empty = Record tags cats quals mods [] [] [] []
        return $ foldr id empty fillers



-- First line
firstLine :: GenParser Char st [Name]
firstLine = (parser `sepBy` designator Y.comma) <* eol



data Annotation = Cat Category | Qual Qualifier | Mod Modifier
partition :: [Annotation] -> ([Category], [Qualifier], [Modifier])
partition xs = ([y | Cat y <- xs], [y | Qual y <- xs], [y | Mod y <- xs])

-- Second line
secondLine :: GenParser Char st ([Category], [Qualifier], [Modifier])
secondLine = partition <$> (annotation `manyTill` eol) where
    annotation = try (Cat <$> parser)
             <|> try (Qual <$> parser)
             <|> (Mod <$> parser)
             <?> "annotation (category|qualifier|prefix|suffix|trail)"


-- Body
fill :: GenParser Char st (Record -> Record)
fill = try (addSec <$> section)
   <|> try (addAttr <$> parser)
   <|> try (addEvent <$> parser)
   <|> try (addApp <$> parser) where
    addSec   x r = r{units=x ++ units r}
    addAttr  x r = r{attrs=x:attrs r}
    addEvent x r = r{events=x:events r}
    addApp   x r = r{appearances=x:appearances r}
