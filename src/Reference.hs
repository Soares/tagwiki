module Reference
    ( Reference(..)
    , Category(..)
    , Qualifier(..)
    ) where
import Data.Either
import Data.Functor
import Modifier
import Parsing
import qualified Symbols as Y
import Tag hiding ( tag )
import Text.Printf
import Text.ParserCombinators.Parsec


-- A reference to another file and/or event
data Reference = Ref { tag        :: Tag
                     , categories :: [Category]
                     , qualifiers :: [Qualifier]
                     , events     :: [Event]
                     } deriving Eq
instance Show Reference where
    show (Ref t cs qs es) = printf "%s%s%s%s" (show t)
        (concatMap show cs) (concatMap show qs) (concatMap show es)
instance Parseable Reference where
    parser = do
        text <- parser
        (cats, quals) <- partitionEithers <$> many catOrQual
        evs <- many parser
        optional (designator Y.halt)
        return $ Ref text cats quals evs

data Category = Cat String deriving Eq
instance Show Category where
    show (Cat s) = printf "#%s" s
instance Parseable Category where
    parser = Cat <$> (hash >> modifier)


data Qualifier = Qual String deriving Eq
instance Show Qualifier where
    show (Qual s) = printf "(%s)" s
instance Parseable Qualifier where
    parser = Qual <$> (between oparen cparen modifier)


data Event = Event String deriving Eq
instance Show Event where
    show (Event s) = printf "!%s" s
instance Parseable Event where
    parser = Event <$> (bang >> (except Tag.restricted))
 

oparen, cparen, hash, bang :: GenParser Char st ()
hash = designator Y.category
oparen = designator Y.oQualifier
cparen = designator Y.cQualifier
bang = designator Y.event

modifier :: GenParser Char st String
modifier = except $ concat [Y.comma, Tag.restricted, Modifier.restricted]

catOrQual :: GenParser Char st (Either Category Qualifier)
catOrQual = try (Left <$> parser) <|> (Right <$> parser)
        <?> "category or qualifier"
