module Text.Reference
    ( Reference(..)
    , source
    , tag
    ) where
import Control.Applicative ( (<*), (<$>) )
import Control.Monad.Reader
import Data.Either
import Data.List hiding ( find )
import Database ( Database )
import qualified Database
import File
import Text.DateTime.Moment
import Text.Fragment
import Text.Modifier ( category, qualifier )
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
locate :: Reference -> Reader Database (Maybe File)
locate ref = Database.file (categories ref) (qualifiers ref) (text ref)

-- Resolution to date
instance Dateable Reference where
    date ref = do
        file <- asks locate ref
        let event = (flip pinpoint $ events ref) =<< file
        case event of
            Nothing -> return $ Unknown $ printf
                "Could not pinpoint %s in %s"
                (intercalate "!" (events ref))
                (show file)
            Just ev -> date ev
        
-- Resolution to string
instance Fragment Reference where
    resolve ref = asks locate ref >>= \file -> return $ case file of
        Nothing -> printf "ERROR: %s not found" (show ref)
        Just f -> show f

-- Source file lookup
source :: Reference -> Reader Database (Maybe String)
source ref = asks locate ref >>= \file -> return $ case file of
    Nothing -> Nothing
    Just f -> Just $ reference f (events ref)


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
        (if null cs then "" else '#':intercalate "#" cs)
        (if null qs then "" else '(':intercalate ")(" qs ++ ")")
        (if null es then "" else '!':intercalate "!" es)
