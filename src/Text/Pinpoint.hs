{-# LANGUAGE FlexibleInstances #-}
module Text.Pinpoint ( Pinpoint(pin, point) ) where
import Control.Applicative hiding ( (<|>), empty )
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.TagWiki
import Text.Pin
import Text.Point

data Pinpoint = Pinpoint
    { pin   :: Pin
    , point :: Maybe Point
    } deriving (Eq, Ord)

instance Show Pinpoint where
    show (Pinpoint p Nothing) = show p
    show (Pinpoint p (Just pt)) = show p ++ show pt

instance Parseable Pinpoint where
    parser = try (Pinpoint <$> parser <*> jpt)
         <|> try (Pinpoint empty <$> jpt)
         <|> (flip Pinpoint Nothing <$> parser)
         <?> "pinpoint"
        where jpt = Just <$> (whitespace *> parser)
