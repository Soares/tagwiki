{-# LANGUAGE FlexibleInstances #-}
module Control.DateTime.Expression ( Expression(..), Expression2(..) ) where
import Control.Applicative hiding ( (<|>) )
import Control.DateTime.Absolute
import Control.DateTime.Relative
-- TODO: remove Sink?
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.TagWiki
import Text.Pinpoint
import Text.Printf
import qualified Text.Symbols as Y

data RelExpr = RelPlus RelExpr RelExpr
             | RelMinus RelExpr RelExpr
             | RelClobber RelExpr RelExpr
             | Rel Relative
             deriving Eq

data AbsExpr = Plus Absolute RelExpr
             | Minus Absolute RelExpr
             | Clobber Absolute RelExpr
             | From Pinpoint
             | Abs Absolute

-- The means by which to resolve a date
data Expression = Plus Expression Expression
                | Minus Expression Expression
                | Clobber Expression Expression
                | From Pinpoint
                | Abs Absolute
                | Rel Relative
                deriving Eq

instance Show Expression where
    show (Plus left right) = printf "%s + %s" (show left) (show right)
    show (Minus left right) = printf "%s + %s" (show left) (show right)
    show (Clobber left right) = printf "%s ⇐ %s" (show left) (show right)
    show (From p) = show p
    show (Abs a) = show a
    show (Rel r) = show r

instance Parseable Expression where
    parser = operations `chainl1` (whitespace >> return Clobber)

data Expression2 = Simply Expression
                 | More Expression
                 | Less Expression
                 | Now
                 deriving Eq

instance Show Expression2 where
    show (Simply x) = show x
    show (More x) = '+':show x
    show (Less x) = '-':show x
    show Now = "«present»"

instance Parseable Expression2 where
    parser = try (More <$> (add >> parser))
         <|> try (Less <$> (sub >> parser))
         <|> try (Simply <$> parser)
         <|> (whitespace >> return Now)
         <?> "second date in range"

-- Operators that only occur in expressions
add, sub, oparen, cparen :: GenParser Char st ()
add = operator Y.addDate
sub = operator Y.subDate
oparen = operator Y.oParen
cparen = operator Y.cParen

operations :: GenParser Char st Expression
operations = term `chainl1` addsub where
    addsub = try (add >> return Plus)
         <|> (sub >> return Minus)
         <?> "+/- date expression"

-- 'simple' terms
term :: GenParser Char st Expression
term = try (between oparen cparen parser)
   <|> try (Abs <$> floating parser)
   <|> try (Rel <$> floating parser)
   <|> (From <$> floating parser)
   <?> "simple date expression"
