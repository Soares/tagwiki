module Control.Reference
    ( Reference(..)
    , source
    , tag
    ) where
import Control.Applicative ( (<*), (<$>) )
import Control.Monad.Reader
import Control.Dangerous hiding ( Warning )
import Data.Either
import Data.List hiding ( find )
import Data.Directory ( Directory, Operation )
import qualified Data.Directory as Directory
import Data.File ( File )
import qualified Data.File as File
import Control.DateTime.Moment
import Text.Fragment
import Control.Modifier ( category, qualifier )
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.TagWiki
import Text.Printf
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

locate :: Reference -> Reader Directory (Maybe File)
locate ref = Directory.file (categories ref) (qualifiers ref) (text ref)

operate :: Reference -> a -> (File -> Operation a) -> Operation a
operate ref no fn = lift (asks locate ref) >>= \r -> case r of
    Nothing -> warn (CantFind ref) >> return no
    Just file -> fn file

-- Resolution to date
instance Dateable Reference where
    date ref = operate ref unknown (File.pinpoint $ events ref)
        where unknown = Unknown $ printf "Can't pinpoint %s" (show ref)
        
-- Resolution to string
instance Fragment Reference where resolve = show

-- Source file lookup
source :: Reference -> Operation String
source ref = operate ref "" $ return . File.reference (events ref)


-- Parsing
tag :: GenParser Char st String
tag = except Y.restrictedInRefs <* optional halt

instance Parseable Reference where
    parser = do
        txt <- tag
        (cats, quals) <- partitionEithers <$> many catOrQual
        evs <- many (bang >> except Y.restrictedInRefs)
        optional (designator Y.halt)
        return $ Ref txt cats quals evs

halt :: GenParser Char st ()
halt = whitespace >> string Y.halt >> whitespace >> return ()

bang :: GenParser Char st ()
bang = designator Y.event

catOrQual :: GenParser Char st (Either String String)
catOrQual = try (Left <$> category)
        <|> (Right <$> qualifier)
        <?> "category or qualifier"



-- Showing
instance Show Reference where
    show (Ref t cs qs es) = printf "%s%s%s%s" (show t)
        (if null cs then "" else " #"++intercalate "#" cs)
        (if null qs then "" else " ("++intercalate ") (" qs ++ ")")
        (if null es then "" else " !"++intercalate "!" es)
