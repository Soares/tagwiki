module Tag where
import Control.Applicative ( (<*) )
import Data.Functor
import Parsing
import qualified Symbols as Y
import Text.ParserCombinators.Parsec

restricted :: String
restricted = concat [ Y.oLink, Y.cLink, Y.halt, Y.oQualifier, Y.cQualifier
                    , Y.event, Y.category, Y.oDate, Y.cDate, Y.dateRangeSep
                    , Y.addDate, Y.subDate, Y.startDate, "\n"]

newtype Tag = Tag { tag :: String } deriving Eq
instance Show Tag where show (Tag s) = s
instance Parseable Tag where
    parser = Tag <$> ((except restricted) <* (optional halt))

halt :: GenParser Char st ()
halt = whitespace >> string Y.halt >> whitespace >> return ()
