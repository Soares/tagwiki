module Text.Record where
import Control.Applicative ( (<*), (<$>), (<*>) )
import Control.Monad
import Text.ParserCombinators.TagWiki
import Text.ParserCombinators.Parsec
import Text.Appearance ( Appearance )
import Text.Attribute ( Attribute )
import Text.Event ( Event )
import Text.Modifier ( Modifier, partition )
import Text.Reference ( tag )
import Text.Unit ( section, Unit )
import qualified Text.Symbols as Y

-- A whole file
data Record = Record { names       :: [(Bool, String)]
                     , categories  :: [String]
                     , qualifiers  :: [String]
                     , prefixes    :: [String]
                     , suffixes    :: [String]
                     , trails      :: [String]
                     , attrs       :: [Attribute]
                     , events      :: [Event]
                     , appearances :: [Appearance]
                     , units       :: [Unit]
                     } deriving (Eq, Show)
instance Parseable Record where
    parser = do
        tags <- firstLine
        (cats, qals, pres, sufs, trls) <- partition <$> secondLine
        fillers <- fill `manyTill` eof
        let empty = Record tags cats qals pres sufs trls [] [] [] []
        return $ foldr id empty fillers



-- First line
firstLine :: GenParser Char st [(Bool, String)]
firstLine = (name `sepBy` designator Y.comma) <* eol where
    name= whitespace >> (,) <$> priority <*> str where
        priority = option False (pri >> return True)
        escPri = hack >> pri
        str = liftM2 (++) (option "" escPri) tag
        pri = string Y.priority



-- Second line
secondLine :: GenParser Char st [Modifier]
secondLine = parser `manyTill` eol


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
