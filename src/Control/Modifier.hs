module Control.Modifier
    ( Modifier(..)
    , partition
    , category
    , qualifier
    , prefix
    , suffix
    , trail
    ) where
import Control.Applicative ( (<$>) )
import Data.String.Utils
import Text.ParserCombinators.TagWiki
import Text.ParserCombinators.Parsec
import qualified Text.Symbols as Y

data Modifier = Cat String
              | Qal String
              | Pre String
              | Suf String
              | Trl String
              deriving (Eq, Show)

instance Parseable Modifier where
    parser = try (Cat <$> category)
         <|> try (Qal <$> qualifier)
         <|> try (Pre <$> prefix)
         <|> try (Suf <$> suffix)
         <|> (Trl <$> trail)
         <?> "any modifier"

partition :: [Modifier] -> ([String], [String], [String], [String], [String])
partition m = ( [c | Cat c <- m]
              , [q | Qal q <- m]
              , [p | Pre p <- m]
              , [s | Suf s <- m]
              , [t | Trl t <- m] )

restricted :: String
restricted = Y.restrictedInRefs ++ Y.restrictedInMods

category, qualifier, prefix, suffix, trail :: GenParser Char st String
-- External to tags
category = strip <$> (hash >> except restricted)
qualifier = strip <$> between oparen cparen (except restricted)
-- Internal to tags
prefix = strip <$> (carat >> except Y.restrictedInMods)
suffix = strip <$> (dollar >> except Y.restrictedInMods)
trail = strip <$> (comma >> except Y.restrictedInMods)

oparen, cparen, hash, comma, carat, dollar :: GenParser Char st ()
comma = designator Y.comma
carat = designator Y.prefix
dollar = designator Y.suffix
hash = designator Y.category
oparen = designator Y.oQualifier
cparen = designator Y.cQualifier
