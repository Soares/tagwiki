module Text.DateTime.Calculation ( Calculation(..) ) where
import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.TagWiki
import Text.Printf
import Text.DateTime.Expression
import qualified Text.Symbols as Y

data Calculation = Exactly Expression | Range Expression Expression deriving Eq

instance Show Calculation where
    show (Exactly calc) = printf "{%s}" (show calc)
    show (Range left right) = printf "{%s ⋯ %s}" (show left) (show right)

instance Parseable Calculation where
    parser = calculation

-- Parsing
obrace, cbrace, comma :: GenParser Char st ()
obrace = char '{' >> anyWhite >> return ()
cbrace = anyWhite >> char '}' >> return ()
comma = operator Y.comma

calculation, exact, range :: GenParser Char st Calculation
calculation = try exact <|> range <?> "date calculation"
exact = between obrace cbrace (fmap Exactly parser)
range = between obrace cbrace (liftM2 Range parser endExpr)
    where endExpr = comma >> whitespace >> option Present parser
