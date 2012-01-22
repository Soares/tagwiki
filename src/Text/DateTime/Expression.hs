module Text.DateTime.Expression ( Expression(..) ) where
import Data.Functor
import Text.Reference
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.TagWiki
import Text.Printf
import Text.DateTime.AbsDate
import Text.DateTime.RelDate
import Text.DateTime.Time
import qualified Text.Symbols as Y



-- The means by which to resolve a date
data Expression = Plus Expression Expression
          | Minus Expression Expression
          | More Expression
          | Less Expression
          | Present
          | Unknown
          | Clobber Expression Expression
          | From Reference
          | Abs AbsDate
          | Rel RelDate
          | At Time
          deriving Eq
instance Show Expression where
    show (Plus left right) = printf "%s + %s" (show left) (show right)
    show (Minus left right) = printf "%s + %s" (show left) (show right)
    show (More calc) = printf "+ %s" (show calc)
    show (Less calc) = printf "- %s" (show calc)
    show Present = "«present»"
    show Unknown = "«unknown»"
    show (Clobber left right) = printf "%s ⇐ %s" (show left) (show right)
    show (From ref) = show ref
    show (Abs date) = show date
    show (Rel date) = show date
    show (At time) = show time
instance Parseable Expression where
    parser = operations `chainl1` (whitespace >> return Clobber)


-- Operators that only occur in expressions
plus, minus, questionMark, oparen, cparen :: GenParser Char st ()
plus = operator Y.addDate
minus = operator Y.subDate
questionMark = operator Y.unknownDate
oparen = operator Y.oParen
cparen = operator Y.cParen


-- Left-recursive +/-
operations :: GenParser Char st Expression
operations = term `chainl1` addsub where
    addsub = try (plus >> return Plus)
         <|> (minus >> return Minus)
         <?> "+/- date expression"

-- 'simple' terms
term :: GenParser Char st Expression
term = try (between oparen cparen parser)
   <|> try (More <$> (plus >> parser))
   <|> try (Less <$> (minus >> parser))
   <|> try (questionMark >> return Unknown)
   <|> try (Abs <$> floating parser)
   <|> try (Rel <$> floating parser)
   <|> try (At  <$> floating parser)
   <|> try (Rel . fromYear <$> floating number)
   <|> try (From <$> floating parser)
   <?> "simple date expression"
