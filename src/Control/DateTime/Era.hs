module Control.DateTime.Era where
import Control.DateTime.Offset hiding ( era )
import Control.DateTime.Utils
import Data.Functor
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.TagWiki
import {-# SOURCE #-} Data.Directory

data Era = Root String
         | Offset Calculation Era

data Era = Era String deriving Eq

instance Show Era where
    show (Era s) = s

instance Parseable Era where
    parser = Era <$> many1 letter

present :: Era
present = Era ""

relatable :: Era -> Era -> Operation Bool
relatable (Era a) (Era b) = do
    as <- eraOffsets a
    bs <- eraOffsets b
    return $ root (known as) == root (known bs)

difference :: Era -> Era -> Operation [Int]
difference a b = do
    as <- fromRoot a
    bs <- fromRoot b
    return $ zipAll (-) as bs

fromRoot :: Era -> Operation [Int]
fromRoot (Era e) = do
    offsets <- eraOffsets e
    let diffable = known offsets
    return $ foldr (zipAll (+) . diff) [] diffable
