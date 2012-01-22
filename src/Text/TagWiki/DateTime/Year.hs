module Text.TagWiki.DateTime.Year where
import Control.Monad
import Data.Functor
import Text.Parser
import Text.ParserCombinators.Parsec
import Text.Printf

data Year = Year Int Era deriving Eq
instance Show Year where
    show (Year y e) = printf "%d%s" y (show e)
instance Parseable Year where
    parser = liftM2 Year number parser

data Era = Era String deriving Eq
instance Show Era where
    show (Era "") = "«no era»"
    show (Era s) = s
instance Parseable Era where
    parser = Era <$> many1 letter
