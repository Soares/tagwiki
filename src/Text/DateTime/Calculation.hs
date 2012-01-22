module Text.DateTime.Calculation ( Calculation(..), beginning, ending ) where
import Control.Applicative ( (<$>), (<*>) )
import {-# SOURCE #-} Database
import Text.Fragment
import Text.DateTime.Moment
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.TagWiki
import Text.Printf
import Text.DateTime.Expression
import qualified Text.Symbols as Y

data Calculation = Exactly Expression | Range Expression Expression2 deriving Eq

beginning :: Calculation -> Operation Moment
beginning (Exactly x) = date x
beginning (Range x _) = date x

ending :: Calculation -> Operation Moment
ending (Exactly x) = date x
ending (Range x y) = date (x, y)

instance Show Calculation where
    show (Exactly calc) = printf "{%s}" (show calc)
    show (Range left right) = printf "{%s ⋯ %s}" (show left) (show right)

instance Parseable Calculation where
    parser = calculation

instance Fragment Calculation where
    resolve _ = return "DATES CAN'T RESOLVE YET"

-- Parsing
obrace, cbrace, comma :: GenParser Char st ()
obrace = char '{' >> anyWhite >> return ()
cbrace = anyWhite >> char '}' >> return ()
comma = operator Y.comma

calculation, exact, range :: GenParser Char st Calculation
calculation = try exact <|> range <?> "date calculation"
exact = between obrace cbrace (Exactly <$> parser)
range = between obrace cbrace (Range <$> parser <*> (comma >> parser))
