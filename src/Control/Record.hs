module Control.Record where
import Control.Applicative ( (<*), (<$>), (<*>) )
import Control.Monad
import Text.ParserCombinators.TagWiki
import Text.ParserCombinators.Parsec
import Control.Appearance ( Appearance )
import Control.Attribute ( Attribute )
import Control.Event ( Event )
import Control.Modifier ( Modifier, partition )
import Control.Reference ( tag )
import Control.Unit ( section, Unit )
import qualified Text.Symbols as Y

{-
data Head = Head { names       :: [(Bool, String)]
                 , categories  :: [String]
                 , qualifiers  :: [String]
                 , prefixes    :: [String]
                 , suffixes    :: [String]
                 , trails      :: [String]
                 } deriving Eq
instance Show Head where
    show (Head ns cs qs ps ss ts) = printf "%s %s%s%s%s%s"
        (intercalate "," $ map $ \(p,t) -> (if p then "+" else "")++t)
        (if null ps then "" else '^':intercalate "^" ps)
        (if null ss then "" else '$':intercalate "$" ss)
        (if null ts then "" else ' ':intercalate "," ts)
        (if null cs then "" else '#':intercalate "#" cs)
        (if null qs then "" else '(':intercalate ")("++")" qs)
        -}
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
