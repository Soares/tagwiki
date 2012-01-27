{-# LANGUAGE FlexibleInstances #-}
module Control.DateTime.Expression ( Expression(..), Expression2(..) ) where
import Control.Applicative ( (<$>), (<*>) )
import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.TagWiki
import Text.Pinpoint
import Text.Printf
import Control.DateTime.AbsDate
import Control.DateTime.Moment hiding ( Unknown, plus, minus )
import qualified Control.DateTime.Moment as Moment
import Control.DateTime.RelDate
import Control.DateTime.Time
import qualified Text.Symbols as Y

-- The means by which to resolve a date
data Expression = Plus Expression Expression
                | Minus Expression Expression
                | Unknown
                | Clobber Expression Expression
                | From Pinpoint
                | Abs AbsDate
                | Rel RelDate
                | At Time
                deriving Eq

instance Show Expression where
    show (Plus left right) = printf "%s + %s" (show left) (show right)
    show (Minus left right) = printf "%s + %s" (show left) (show right)
    show Unknown = "«unknown»"
    show (Clobber left right) = printf "%s ⇐ %s" (show left) (show right)
    show (From pin) = show pin
    show (Abs d) = show d
    show (Rel d) = show d
    show (At t) = show t

instance Parseable Expression where
    parser = operations `chainl1` (whitespace >> return Clobber)

instance Dateable Expression where
    date (Plus x y) = join $ Moment.plus <$> date x <*> date y
    date (Minus x y) = join $ Moment.minus <$> date x <*> date y
    date Unknown = return $ Moment.Unknown ""
    date (Clobber left right) = do
        x <- date left
        y <- date right
        let z = clobber x y
        return z
    date (From pp) = date pp
    date (Abs d) = date d
    date (Rel d) = date d
    date (At t) = date t



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

instance Dateable (Expression, Expression2) where
    date (x, More y) = join $ Moment.plus <$> date x <*> date y
    date (x, Less y) = join $ Moment.minus <$> date x <*> date y
    date (_, Simply x) = date x
    date (_, Present) = return present



-- Operators that only occur in expressions
plus, minus, questionMark, oparen, cparen :: GenParser Char st ()
plus = operator Y.addDate
minus = operator Y.subDate
questionMark = operator Y.unknownDate
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
   <|> try (questionMark >> return Unknown)
   <|> try (Abs <$> floating parser)
   <|> try (Rel <$> floating parser)
   <|> try (At  <$> floating parser)
   <|> try (Rel . fromYear <$> floating number)
   <|> (From <$> floating parser)
   <?> "simple date expression"
