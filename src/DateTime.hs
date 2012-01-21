module DateTime ( Calculation(..) ) where
import DateTime.Expression
import Control.Monad
import Parsing
import qualified Symbols as Y
import Text.ParserCombinators.Parsec
import Text.Printf

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
calculation = try exact <|> try range <?> "date calculation"
exact = between obrace cbrace (fmap Exactly parser)
range = between obrace cbrace (liftM2 Range parser endExpr)
    where endExpr = comma >> whitespace >> option Present parser
