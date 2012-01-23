module Data.Head where
import Control.Applicative ( (<$>), (<*>), (<*) )
import Control.Modifier ( Modifier, partition )
import Control.Monad
import Control.Reference ( tag )
import Data.List ( intercalate )
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.TagWiki
import Text.Printf
import qualified Text.Symbols as Y



data Head = Head { names       :: [(Bool, String)]
                 , categories  :: [String]
                 , qualifiers  :: [String]
                 , prefixes    :: [String]
                 , suffixes    :: [String]
                 , trails      :: [String]
                 } deriving Eq



-- Parsing
instance Parseable Head where
    parser = do
        tags <- firstLine
        (cats, qals, pres, sufs, trls) <- partition <$> secondLine
        return $ Head tags cats qals pres sufs trls


firstLine :: GenParser Char st [(Bool, String)]
firstLine = (name `sepBy` designator Y.comma) <* eol where
    name= whitespace >> (,) <$> priority <*> str where
        priority = option False (pri >> return True)
        escPri = hack >> pri
        str = liftM2 (++) (option "" escPri) tag
        pri = string Y.priority


secondLine :: GenParser Char st [Modifier]
secondLine = parser `manyTill` eol



-- Showing
instance Show Head where
    show (Head ns cs qs ps ss ts) = printf "%s %s%s%s%s%s"
        (intercalate "," (map showTag ns))
        (if null ps then "" else '^':intercalate "^" ps)
        (if null ss then "" else '$':intercalate "$" ss)
        (if null ts then "" else ' ':intercalate "," ts)
        (if null cs then "" else '#':intercalate "#" cs)
        (if null qs then "" else '(':intercalate ")(" qs++")")
        where showTag (p, t) = (if p then "+" else "")++t