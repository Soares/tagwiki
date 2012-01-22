module Text.TagWiki.Reference
    ( Reference(..)
    , Category(..)
    , Qualifier(..)
    , tag
    ) where
import Control.Applicative ( (<*) )
import Data.Either
import Data.Functor
import Text.Parser
import Text.ParserCombinators.Parsec
import Text.Printf
import qualified Text.TagWiki.Modifier as Modifier
import qualified Text.TagWiki.Symbols as Y

tag :: GenParser Char st String
tag = except restricted <* optional halt


-- A reference to another file and/or event
data Reference = Ref { text       :: String
                     , categories :: [Category]
                     , qualifiers :: [Qualifier]
                     , events     :: [Event]
                     } deriving Eq
instance Show Reference where
    show (Ref t cs qs es) = printf "%s%s%s%s" (show t)
        (concatMap show cs) (concatMap show qs) (concatMap show es)
instance Parseable Reference where
    parser = do
        txt <- tag
        (cats, quals) <- partitionEithers <$> many catOrQual
        evs <- many parser
        optional (designator Y.halt)
        return $ Ref txt cats quals evs


data Category = Cat String deriving Eq
instance Show Category where
    show (Cat s) = printf "#%s" s
instance Parseable Category where
    parser = Cat <$> (hash >> modifier)


data Qualifier = Qual String deriving Eq
instance Show Qualifier where
    show (Qual s) = printf "(%s)" s
instance Parseable Qualifier where
    parser = Qual <$> between oparen cparen modifier


data Event = Event String deriving Eq
instance Show Event where
    show (Event s) = printf "!%s" s
instance Parseable Event where
    parser = Event <$> (bang >> except restricted)
 

oparen, cparen, hash, bang :: GenParser Char st ()
hash = designator Y.category
oparen = designator Y.oQualifier
cparen = designator Y.cQualifier
bang = designator Y.event

modifier :: GenParser Char st String
modifier = except $ restricted ++ Modifier.restricted

catOrQual :: GenParser Char st (Either Category Qualifier)
catOrQual = try (Left <$> parser) <|> (Right <$> parser)
        <?> "category or qualifier"

restricted :: String
restricted = concat [ Y.oLink, Y.cLink, Y.halt, Y.oQualifier, Y.cQualifier
                    , Y.event, Y.category, Y.oDate, Y.cDate, Y.dateRangeSep
                    , Y.addDate, Y.subDate, Y.startDate, "\n"]

halt :: GenParser Char st ()
halt = whitespace >> string Y.halt >> whitespace >> return ()
