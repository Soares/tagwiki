module Text.Event ( Event(..) ) where
import Control.Monad
import Data.Functor
import Text.ParserCombinators.TagWiki
import Text.ParserCombinators.Parsec
import Text.Printf
import Text.DateTime.Calculation
import Text.DateTime.Expression
import Text.Reference ( tag )
import Text.Unit
import qualified Text.Symbols as Y

data Event = Event { name :: String
                   , when :: Calculation
                   , text :: [Unit]
                   } deriving Eq

instance Show Event where
    show (Event k w xs) = printf "!%s @%s %s" (show k) (show w)
        (if null xs then "" else "...")

instance Parseable Event where
    parser = marker Y.event >> liftM3 Event tag date block

date :: GenParser Char st Calculation
date = try parser
   <|> try (Exactly <$> liftM2 Clobber (Abs <$> parser) (At <$> parser))
   <|> (Exactly <$> (Abs <$> parser))
   <?> "date for event"
