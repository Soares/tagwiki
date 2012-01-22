module Text.TagWiki.Modifier
    ( Modifier(..)
    , prefixes
    , suffixes
    , trails
    , separate
    , restricted
    ) where
import Data.Functor
import Text.Parser
import Text.ParserCombinators.Parsec
import qualified Text.TagWiki.Symbols as Y

-- Modifiers on tags
data Modifier = Prefix String
              | Suffix String
              | Trail String
              deriving Eq

instance Show Modifier where
    show (Prefix s) = '^':s
    show (Suffix s) = '$':s
    show (Trail s) = ',':s

instance Parseable Modifier where
    parser = try (Prefix <$> (carat >> except restricted))
         <|> try (Suffix <$> (dollar >> except restricted))
         <|> (Trail <$> (comma >> except restricted))
         <?> "Modifier"

-- Modifier partitioners
prefixes, suffixes, trails :: [Modifier] -> [String]
prefixes xs = [y | Prefix y <- xs]
suffixes xs = [y | Suffix y <- xs]
trails xs = [y | Trail y <- xs]
separate :: [Modifier] -> ([String], [String], [String])
separate xs = (prefixes xs, suffixes xs, trails xs)

-- Modifier operators
comma, carat, dollar :: GenParser Char st ()
comma = designator Y.comma
carat = designator Y.prefix
dollar = designator Y.suffix

restricted :: String
restricted = concat [Y.category, Y.prefix, Y.suffix, Y.trail
                    , Y.oQualifier, Y.cQualifier, "\n"]
