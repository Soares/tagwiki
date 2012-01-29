module Control.Modifier
    ( Modifier(..)
    , parse
    , partition
    , categories
    , qualifiers
    , prefixes
    , suffixes
    , category
    , qualifier
    , prefix
    , suffix
    -- Common parsers
    , catOrQual
    , anyMod
    ) where
import Control.Applicative ( (<$>) )
import Data.String.Utils
import Text.ParserCombinators.Parsec hiding ( parse )
import Text.ParserCombinators.TagWiki
import qualified Text.Symbols as Y

data Modifier = Cat String
              | Qal String
              | Pre String
              | Suf String
              deriving (Eq, Show)

-- TODO: qualifiers need to be able to parse entire References
parse :: [GenParser Char st Modifier] -> GenParser Char st Modifier
parse = choice . map try

partition :: [Modifier] -> ([String], [String], [String], [String])
partition m = ( categories m
              , qualifiers m
              , prefixes   m
              , suffixes   m )

categories, qualifiers, prefixes, suffixes :: [Modifier] -> [String]
categories m = [c | Cat c <- m]
qualifiers m = [q | Qal q <- m]
prefixes   m = [p | Pre p <- m]
suffixes   m = [s | Suf s <- m]

restricted :: String
restricted = Y.restrictedInRefs ++ Y.restrictedInMods

catOrQual, anyMod :: GenParser Char st Modifier
catOrQual = parse [category, qualifier]
anyMod = parse [category, qualifier, prefix, suffix]

category, qualifier, prefix, suffix :: GenParser Char st Modifier
-- External to tags
category = Cat . strip <$> (hash >> except restricted)
qualifier = Qal . strip <$> between oparen cparen (except restricted)
-- Internal to tags
prefix = Pre . strip <$> (carat >> except Y.restrictedInMods)
suffix = Suf . strip <$> (dollar >> except Y.restrictedInMods)

oparen, cparen, hash, carat, dollar :: GenParser Char st ()
carat = designator Y.prefix
dollar = designator Y.suffix
hash = designator Y.category
oparen = designator Y.oQualifier
cparen = designator Y.cQualifier
