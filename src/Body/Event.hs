module Body.Event ( Event(..) ) where
import Body.Unit
import Control.Monad
import Data.Functor
import DateTime
import DateTime.Expression
import Parsing
import Tag
import Text.ParserCombinators.Parsec
import Text.Printf
import qualified Symbols as Y

data Event = Event { name :: Tag
                   , when :: Calculation
                   , text :: [Unit]
                   } deriving Eq

instance Show Event where
    show (Event k w xs) = printf "!%s @%s %s" (show k) (show w)
        (if null xs then "" else "...")

instance Parseable Event where
    parser = (marker Y.event) >> liftM3 Event parser date block

date :: GenParser Char st Calculation
date = try parser
   <|> try (Exactly <$> liftM2 Clobber (Abs <$> parser) (At <$> parser))
   <|> (Exactly <$> (Abs <$> parser))
   <?> "date for event"
