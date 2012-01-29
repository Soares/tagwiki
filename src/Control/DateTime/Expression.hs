{-# LANGUAGE FlexibleInstances #-}
module Control.DateTime.Expression ( Expression(..), Expression2(..) ) where
-- TODO: add ways to parse moments
-- import Control.AbsDate.Time
-- import Control.DateTime.Time
-- import Control.RelDate.Time
import {-# SOURCE #-} Data.Directory ( pinpoint )
import Control.Applicative hiding ( (<|>) )
import Control.DateTime.Moment ( Moment, Momentus(..), present )
import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.TagWiki
import Text.Pinpoint
import Text.Printf
import qualified Control.DateTime.Moment as Moment
import qualified Text.Symbols as Y

-- The means by which to resolve a date
data Expression = Plus Expression Expression
                | Minus Expression Expression
                | Clobber Expression Expression
                | From Pinpoint
                | DateTime Moment
                deriving Eq

instance Show Expression where
    show (Plus left right) = printf "%s + %s" (show left) (show right)
    show (Minus left right) = printf "%s + %s" (show left) (show right)
    show (Clobber left right) = printf "%s ⇐ %s" (show left) (show right)
    show (From p) = show p
    show (DateTime dt) = show dt

instance Parseable Expression where
    parser = operations `chainl1` (whitespace >> return Clobber)

instance Momentus Expression where
    -- TODO: explore this join/bind relation
    moment (Plus x y) = join $ Moment.plus <$> moment x <*> moment y
    moment (Minus x y) = join $ Moment.minus <$> moment x <*> moment y
    moment (Clobber x y) = Moment.clobber <$> moment x <*> moment y
    moment (From pp) = pinpoint pp
    moment (DateTime dt) = moment dt



data Expression2 = Simply Expression
                 | More Expression
                 | Less Expression
                 | Present
                 deriving Eq

instance Show Expression2 where
    show (Simply x) = show x
    show (More x) = '+':show x
    show (Less x) = '-':show x
    show Present = "«present»"

instance Parseable Expression2 where
    parser = try (More <$> (plus >> parser))
         <|> try (Less <$> (minus >> parser))
         <|> try (Simply <$> parser)
         <|> (whitespace >> return Present)
         <?> "second date in range"

instance Momentus (Expression, Expression2) where
    moment (x, More y) = join $ Moment.plus <$> moment x <*> moment y
    moment (x, Less y) = join $ Moment.minus <$> moment x <*> moment y
    moment (_, Simply x) = moment x
    moment (_, Present) = pure present



-- Operators that only occur in expressions
plus, minus, oparen, cparen :: GenParser Char st ()
plus = operator Y.addDate
minus = operator Y.subDate
oparen = operator Y.oParen
cparen = operator Y.cParen

operations :: GenParser Char st Expression
operations = term `chainl1` addsub where
    addsub = try (plus >> return Plus)
         <|> (minus >> return Minus)
         <?> "+/- date expression"

-- 'simple' terms
term :: GenParser Char st Expression
term = try (between oparen cparen parser)
   <|> try (DateTime <$> floating parser)
   <|> (From <$> floating parser)
   <?> "simple date expression"
