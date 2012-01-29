module Control.DateTime.Calculation
    ( Calculation(..)
    , pinpoint
    , beginning
    , ending ) where
import Control.Applicative hiding ( (<|>) )
import Control.DateTime.Expression
import Control.DateTime.Moment
import Text.Fragment
import Text.Point ( Side(..) )
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.TagWiki
import Text.Printf
import qualified Text.Symbols as Y
import {-# SOURCE #-} Data.Directory ( Momentable )

data Calculation = Exactly Expression
                 | Range Expression Expression2
                 deriving Eq

-- TODO: make DangerousType, add instances of warn and throw
-- TODO: Pattern matching not exhaustive.
pinpoint :: (Momentable m) => Side -> Calculation -> m Moment
pinpoint _ (Exactly x) = moment x
pinpoint End (Range x y) = moment (x, y)
pinpoint _ (Range x _) = moment x

beginning :: (Momentable m) => Calculation -> m Moment
beginning (Exactly x) = moment x
beginning (Range x _) = moment x

ending :: (Momentable m) => Calculation -> m Moment
ending (Exactly x) = moment x
ending (Range x y) = moment (x, y)

instance Show Calculation where
    show (Exactly calc) = printf "{%s}" (show calc)
    show (Range left right) = printf "{%s ⋯ %s}" (show left) (show right)

instance Parseable Calculation where
    parser = calculation

instance Fragment Calculation where
    resolve (Exactly calc) = show <$> moment calc
    resolve (Range left right) = rng <$> moment left <*> moment (left, right)
        where rng x y = printf "%s, %s" (show x) (show y)

-- Parsing
obrace, cbrace, comma :: GenParser Char st ()
obrace = char '{' >> anyWhite >> return ()
cbrace = anyWhite >> char '}' >> return ()
comma = operator Y.comma

calculation, exact, range :: GenParser Char st Calculation
calculation = try exact <|> range <?> "date calculation"
exact = between obrace cbrace (Exactly <$> parser)
range = between obrace cbrace (Range <$> parser <*> (comma >> parser))
