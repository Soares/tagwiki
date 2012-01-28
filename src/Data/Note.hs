module Data.Note where
import Control.Applicative hiding ( (<|>) )
import Data.Set ( fromList )
import Data.Body
import Data.List
import Data.Record ( Record )
import Data.Either
import Text.Tag ( tag )
import Control.Modifier
import Text.ParserCombinators.TagWiki
import Text.ParserCombinators.Parsec
import Text.Printf
import qualified Text.Symbols as Y
import qualified Data.Record as Record

data Note = Note { names      :: [(Bool, String)]
                 , tags       :: [String]
                 , categories :: [String]
                 , qualifiers :: [String]
                 , body       :: Body
                 }

instance Record Note where
    note = id

instance Eq Note where
    x == y = ns && qs where
        ns = fromList (names x) == fromList (names y)
        qs = fromList (qualifiers x) == fromList (qualifiers y)

instance Parseable Note where
    parser = do
        ns <- firstLine
        (cs, qs) <- partitionEithers <$> secondLine
        b <- parser
        pure Note{ names = ns
                 , tags = map (makeTag qs . snd) ns
                 , categories = cs
                 , qualifiers = qs
                 , body = b }

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

secondLine :: GenParser Char st [Either String String]
secondLine = catOrQual `manyTill` (whitespace *> eol) where
    catOrQual = try (Left <$> category)
            -- TODO: testing only
            <|> (Right <$> prefix)
            <|> (Right <$> suffix)
            <|> (Right <$> trail)
            -- ENDTODO
            <|> (Right <$> qualifier)
            <?> "category or qualifier"
