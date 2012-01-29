module Control.Modifier
    ( Modifier(..)
    , parse
    , partition
    , categories
    , qualifiers
    , prefixes
    , suffixes
    , trails
    , category
    , qualifier
    , prefix
    , suffix
    , trail
    ) where
import Control.Applicative ( (<$>) )
import Data.String.Utils
import Text.ParserCombinators.TagWiki
import Text.ParserCombinators.Parsec hiding ( parse )
import qualified Text.Symbols as Y

data Modifier = Cat String
              | Qal String
              | Pre String
              | Suf String
              -- TODO: remove trails
              | Trl String
              deriving (Eq, Show)

parse :: [GenParser Char st Modifier] -> GenParser Char st Modifier
parse = choice . map try

partition :: [Modifier] -> ([String], [String], [String], [String], [String])
partition m = ( categories m
              , qualifiers m
              , prefixes   m
              , suffixes   m
              , trails     m )

categories, qualifiers, prefixes, suffixes, trails :: [Modifier] -> [String]
categories m = [c | Cat c <- m]
qualifiers m = [q | Qal q <- m]
prefixes   m = [p | Pre p <- m]
suffixes   m = [s | Suf s <- m]
trails     m = [t | Trl t <- m]

restricted :: String
restricted = Y.restrictedInRefs ++ Y.restrictedInMods

category, qualifier, prefix, suffix, trail :: GenParser Char st Modifier
-- External to tags
category = Cat . strip <$> (hash >> except restricted)
qualifier = Qal . strip <$> between oparen cparen (except restricted)
-- Internal to tags
prefix = Pre . strip <$> (carat >> except Y.restrictedInMods)
suffix = Suf . strip <$> (dollar >> except Y.restrictedInMods)
trail = Trl . strip <$> (comma >> except Y.restrictedInMods)

oparen, cparen, hash, comma, carat, dollar :: GenParser Char st ()
comma = designator Y.comma
carat = designator Y.prefix
dollar = designator Y.suffix
hash = designator Y.category
oparen = designator Y.oQualifier
cparen = designator Y.cQualifier
