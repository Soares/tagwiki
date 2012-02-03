module Control.DateTime.Calculation
    ( Calculation(..)
    , when
    ) where
import Control.Applicative hiding ( (<|>) )
import Control.DateTime.Absolute hiding ( expression )
import Control.DateTime.Relative hiding ( expression, plus, minus, clobber )
import Internal
import Text.Fragment
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.TagWiki
import Text.Point
import Text.Pinpoint
import Text.Printf
import qualified Text.Symbols as Y
import Control.DateTime.Absolute as Absolute
import Control.DateTime.Relative as Relative hiding ( plus, minus, clobber )

data Calculation = Exactly Expression
                 | Range Expression Expression2
                 deriving Eq

data Expression = Abs Absolute
                | Clobber Pinpoint Relative
                | Plus Pinpoint Relative
                | Minus Pinpoint Relative
                | PP Pinpoint
                deriving Eq

data Expression2 = Simply Expression
                 | More Relative
                 | Less Relative
                 deriving Eq

instance Show Expression where
    show (Abs a) = show a
    show (Plus x y) = printf "%s + %s" (show x) (show y)
    show (Minus x y) = printf "%s - %s" (show x) (show y)
    show (Clobber x y) = printf "%s | %s" (show x) (show y)
    show (PP pp) = show pp

instance Show Expression2 where
    show (Simply x) = show x
    show (More x) = '+':show x
    show (Less x) = '-':show x

instance Parseable Expression where
    parser = try (Abs <$> Absolute.expression)
         <|> try (Clobber <$> parser <*> Relative.expression)
         <|> try (Plus <$> parser <*> (add *> Relative.expression))
         <|> try (Minus <$> parser <*> (sub *> Relative.expression))
         <|> try (PP <$> parser)
         <?> "a date calculation opening date"

instance Parseable Expression2 where
    parser = try (More <$> (operator Y.addDate *> Relative.expression))
         <|> try (Less <$> (operator Y.subDate *> Relative.expression))
         <|> (whitespace *> pure (Simply $ Abs Present))
         <|> try (Simply . PP <$> parser)
         <?> "a date calculation closing date"

instance Show Calculation where
    show (Exactly a) = printf "{%s}" (show a)
    show (Range left right) = printf "{%s → %s}" (show left) (show right)

instance Parseable Calculation where
    parser = try exact <|> range <?> "date calculation" where
        obrace = char '{' >> anyWhite >> return ()
        cbrace = anyWhite >> char '}' >> return ()
        comma = operator Y.comma
        exact = between obrace cbrace (Exactly <$> parser)
        range = between obrace cbrace (Range <$> parser <*> (comma *> parser))

instance Fragment Calculation where
    resolve (Exactly x) = show <$> when Auto x
    resolve (Range x y) = do
        wx <- when Auto x
        wy <- whenEnd y =<< when Auto x
        pure $ printf "%s → %s" (show wx) (show wy)

instance Momented Calculation where
    when End (Range x y) = when End x >>= whenEnd y
    when s (Range x _) = when s x
    when s (Exactly x) = when s x

instance Momented Expression where
    when _ (Abs a) = pure a
    when _ (Plus pp rel) = flip plus rel <$> pinpoint pp
    when _ (Minus pp rel) = flip minus rel <$> pinpoint pp
    when _ (Clobber pp rel) = flip clobber rel <$> pinpoint pp
    when _ (PP pp) = pinpoint pp

whenEnd :: (Internal i) => Expression2 -> Absolute -> i Absolute
whenEnd (Simply x) _ = when Auto x
whenEnd (More rel) x = pure $ plus x rel
whenEnd (Less rel) x = pure $ minus x rel

add, sub :: GenParser Char st ()
add = operator Y.addDate
sub = operator Y.subDate
