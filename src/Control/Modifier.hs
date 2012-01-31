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
import Text.Utils
import qualified Text.Symbols as Y

data (Parseable p) => Modifier p
    = Cat String
    | Qal p
    | Pre String
    | Suf String
    deriving (Eq, Show)

parse :: (Parseable p) => [GenParser Char st (Modifier p)] ->
    GenParser Char st (Modifier p)
parse = choice . map try

partition :: (Parseable p) => [Modifier p] -> ([String], [p], [String], [String])
partition m = ( categories m
              , qualifiers m
              , prefixes   m
              , suffixes   m )

categories, prefixes, suffixes :: (Parseable p) => [Modifier p] -> [String]
qualifiers:: (Parseable p) => [Modifier p] -> [p]
categories m = [c | Cat c <- m]
qualifiers m = [q | Qal q <- m]
prefixes   m = [p | Pre p <- m]
suffixes   m = [s | Suf s <- m]

restricted :: String
restricted = Y.restrictedInRefs ++ Y.restrictedInMods

catOrQual, anyMod :: (Parseable p) => GenParser Char st (Modifier p)
catOrQual = parse [category, qualifier]
anyMod = parse [category, qualifier, prefix, suffix]

category, qualifier, prefix, suffix :: (Parseable p) =>
    GenParser Char st (Modifier p)
-- External to tags
category = Cat . slugify <$> (hash >> except restricted)
qualifier = Qal <$> between oparen cparen parser
-- Internal to tags
prefix = Pre . strip <$> (carat >> except Y.restrictedInMods)
suffix = Suf . strip <$> (dollar >> except Y.restrictedInMods)

oparen, cparen, hash, carat, dollar :: GenParser Char st ()
carat = designator Y.prefix
dollar = designator Y.suffix
hash = designator Y.category
oparen = designator Y.oQualifier
cparen = designator Y.cQualifier
