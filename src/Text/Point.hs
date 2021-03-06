module Text.Point
    ( Point(..)
    , Side(Start, End, Auto)
    , tag
    ) where
import Control.Applicative hiding ( (<|>), many )
import Data.String.Utils ( strip )
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.TagWiki
import Text.Utils
import qualified Text.Symbols as Y

data Side = Start | End | Auto deriving (Eq, Ord)

data Point = Point
    { side :: Side
    , name :: String
    }

tag :: Point -> String
tag = normalize . name

instance Eq Point where
    x == y = side x == side y && tag x == tag y

instance Ord Point where
    x <= y = side x <= side y && tag x <= tag y

instance Parseable Side where
    parser = try (bang >> pure Auto)
         <|> try (carat >> pure Start)
         <|> (dollar >> pure End) where
        bang = marker Y.event
        carat = marker Y.prefix
        dollar = marker Y.suffix

instance Parseable Point where
    parser = Point <$> parser <*> body where
        body = strip <$> many (escaping Y.restrictedInRefs)

instance Show Side where
    show Auto = "!"
    show Start = "^"
    show End = "$"

instance Show Point where
    show (Point End "") = "$end"
    show (Point _ "") = "^start"
    show (Point s str) = show s ++ str
