module DateTime.Year where
import Control.Monad
import Data.Functor
import Parsing
import Text.ParserCombinators.Parsec
import Text.Printf

data Year = Year Int Era deriving Eq
instance Show Year where
    show (Year y e) = printf "%d%s" y (show e)

parseYear :: GenParser Char st Year
parseYear = liftM2 Year number parseEra

data Era = Era String deriving Eq
instance Show Era where
    show (Era "") = "«no era»"
    show (Era s) = s

parseEra :: GenParser Char st Era
parseEra = Era <$> many1 letter
