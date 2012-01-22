module Text.DateTime.Era where
import Control.Monad.Reader
import Data.Functor
import {-# SOURCE #-} Database
import Text.DateTime.Offset hiding ( era )
import Text.DateTime.Utils
import Text.ParserCombinators.TagWiki
import Text.ParserCombinators.Parsec

data Era = Era String deriving Eq

instance Show Era where
    show (Era "") = "«no era»"
    show (Era s) = s

instance Parseable Era where
    parser = Era <$> many1 letter

present :: Era
present = Era ""

relatable :: Era -> Era -> Reader Database Bool
relatable (Era a) (Era b) = do
    as <- asks era a
    bs <- asks era b
    return $ root (known as) == root (known bs)

difference :: Era -> Era -> Reader Database [Int]
difference a b = do
    as <- asks fromRoot a
    bs <- asks fromRoot b
    return $ zipAll (-) as bs

fromRoot :: Era -> Reader Database [Int]
fromRoot (Era e) = do
    offsets <- asks era e
    let diffable = known offsets
    return $ foldr (zipAll (+) . diff) [] diffable
