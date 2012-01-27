module Text.Point
    ( Point(..)
    , Side(Start, End)
    ) where
import Control.Applicative hiding ( (<|>) )
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.TagWiki
import qualified Text.Symbols as Y



data Side = Start | End
          deriving (Eq, Ord)



data Point = Point { side :: Side
                   , name :: String
                   } deriving (Eq, Ord)


instance Parseable Side where
    parser = try (bang >> pure Start)
         <|> try (carat >> pure Start)
         <|> (dollar >> pure End) where
        bang = designator Y.event
        carat = designator Y.prefix
        dollar = designator Y.suffix


instance Parseable Point where
    parser = Point <$> parser <*> except Y.restrictedInRefs


instance Show Side where
    show Start = "!"
    show End = "$"


instance Show Point where
    show (Point n s) = show n ++ s
