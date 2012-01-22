module Text.DateTime.Calculation ( Calculation(..) ) where
import Control.Applicative ( (<$>), (<*>) )
import Text.DateTime.Moment
import Text.Fragment
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.TagWiki
import Text.Printf
import Text.DateTime.Expression
import qualified Text.Symbols as Y

data Calculation = Exactly Expression | Range Expression Expression2 deriving Eq

instance Dateable Calculation where
    date (Exactly x) = date x
    date (Range x y) = date x -- TODO: incorrect

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
