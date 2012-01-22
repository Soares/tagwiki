module Text.TagWiki.Event ( Event(..) ) where
import Control.Monad
import Data.Functor
import Text.Parser
import Text.ParserCombinators.Parsec
import Text.Printf
import Text.TagWiki.DateTime
import Text.TagWiki.DateTime.Expression
import Text.TagWiki.Reference ( tag )
import Text.TagWiki.Unit
import qualified Text.TagWiki.Symbols as Y

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
