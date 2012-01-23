module Control.Reference
    ( Reference(..)
    , source
    , tag
    ) where
import Control.Applicative ( pure, (<*), (<$>) )
import Control.DateTime.Moment
import Control.Modifier ( category, qualifier )
import {-# SOURCE #-} Data.Directory
import Data.Either
import Data.List hiding ( find )
import Data.String.Utils ( strip )
import Text.Fragment
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.TagWiki
import Text.Printf
import {-# SOURCE #-} qualified Data.Directory as Dir
import qualified Text.Symbols as Y


-- A reference to another file and/or event
data Reference = Ref { text       :: String
                     , categories :: [String]
                     , qualifiers :: [String]
                     , events     :: [String]
                     } deriving Eq



-- Resolution to file
data Warning = CantFind Reference
instance Show Warning where
    show (CantFind ref) = printf "File not found: %s" (show ref)

-- Resolution to date
instance Dateable Reference where date = Dir.pinpoint
        
-- Resolution to string
instance Fragment Reference where resolve = pure . show

-- Source file lookup
source :: Reference -> Operation String
source = Dir.location


-- Parsing
tag :: GenParser Char st String
tag = except Y.restrictedInRefs <* optional halt

instance Parseable Reference where
    parser = do
        txt <- tag
        (cats, quals) <- partitionEithers <$> many catOrQual
        evs <- many (bang >> except Y.restrictedInRefs)
        optional (designator Y.halt)
        pure $ Ref txt cats quals evs

halt :: GenParser Char st ()
halt = whitespace >> string Y.halt >> whitespace >> pure ()

bang :: GenParser Char st ()
bang = designator Y.event

catOrQual :: GenParser Char st (Either String String)
catOrQual = try (Left <$> category)
        <|> (Right <$> qualifier)
        <?> "category or qualifier"



-- Showing
instance Show Reference where
    show (Ref t cs qs es) = printf "%s%s%s%s" (strip t)
        (if null cs then "" else " #"++intercalate "#" cs)
        (if null qs then "" else " ("++intercalate ") (" qs ++ ")")
        (if null es then "" else " !"++intercalate "!" es)
