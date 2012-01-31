{-# LANGUAGE FlexibleInstances #-}
module Control.DateTime.Calculation ( Calculation(..) ) where
import Control.Applicative hiding ( (<|>) )
import Control.DateTime.Expression
import Control.DateTime.Moment
import Text.Fragment
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.TagWiki
import Text.Point ( Side(..) )
import Text.Printf
import qualified Control.DateTime.Moment as Moment
import qualified Text.Symbols as Y

data Calculation = Exactly Expression
                 | Range Expression Expression2
                 deriving Eq

instance Momentus (Side, Calculation) where
    moment (End, Range x y) = moment (x, y)
    moment (_, Range x _) = moment x
    moment (_, Exactly x) = moment x

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
