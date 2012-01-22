module Text.Record where
import Control.Applicative ( (<*) )
import Control.Monad
import Data.Functor
import Text.ParserCombinators.TagWiki
import Text.ParserCombinators.Parsec
import Text.Appearance ( Appearance )
import Text.Attribute ( Attribute )
import Text.Event ( Event )
import Text.Modifier ( Modifier )
import Text.Reference ( tag, Category, Qualifier )
import Text.Unit ( section, Unit )
import qualified Text.Symbols as Y

-- A whole file
data Record = Record { names       :: [(Bool, String)]
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
firstLine :: GenParser Char st [(Bool, String)]
firstLine = (name `sepBy` designator Y.comma) <* eol where
    name= whitespace >> liftM2 (,) priority str where
        priority = option False (pri >> return True)
        escPri = hack >> pri
        str = liftM2 (++) (option "" escPri) tag
        pri = string Y.priority



-- Second line
data Annotation = Cat Category | Qual Qualifier | Mod Modifier

secondLine :: GenParser Char st ([Category], [Qualifier], [Modifier])
secondLine = partition <$> (annotation `manyTill` eol) where
    partition xs = ([y | Cat y <- xs], [y | Qual y <- xs], [y | Mod y <- xs])
    annotation = try (Cat <$> parser)
             <|> try (Qual <$> parser)
             <|> (Mod <$> parser)
             <?> "annotation (category|qualifier|prefix|suffix|trail)"


-- Body
fill :: GenParser Char st (Record -> Record)
fill = try (addSec <$> section)
   <|> try (addAttr <$> parser)
   <|> try (addEvent <$> parser)
   <|> (addApp <$> parser) where
    addSec   x r = r{units=x ++ units r}
    addAttr  x r = r{attrs=x:attrs r}
    addEvent x r = r{events=x:events r}
    addApp   x r = r{appearances=x:appearances r}
