module Control.DateTime.Calculation
    ( Calculation(..)
    , pinpoint
    , beginning
    , ending ) where
import Control.Applicative ( (<$>), (<*>) )
import Control.DateTime.Expression
import Control.DateTime.Moment
import Text.Fragment
import Text.Point ( Side(..) )
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.TagWiki
import Text.Printf
import qualified Text.Symbols as Y
import {-# SOURCE #-} Data.Directory ( Operation )

data Calculation = Exactly Expression | Range Expression Expression2 deriving Eq

pinpoint :: Side -> Calculation -> Operation Moment
pinpoint _ (Exactly x) = date x
pinpoint Start (Range x _) = date x
pinpoint End (Range x y) = date (x, y)

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
    resolve (Exactly calc) = show <$> date calc
    resolve (Range left right) = printer <$> date left <*> date (left, right)
        where printer x y = printf "%s, %s" (show x) (show y)

-- Parsing
obrace, cbrace, comma :: GenParser Char st ()
obrace = char '{' >> anyWhite >> return ()
cbrace = anyWhite >> char '}' >> return ()
comma = operator Y.comma

calculation, exact, range :: GenParser Char st Calculation
calculation = try exact <|> range <?> "date calculation"
exact = between obrace cbrace (Exactly <$> parser)
range = between obrace cbrace (Range <$> parser <*> (comma >> parser))
