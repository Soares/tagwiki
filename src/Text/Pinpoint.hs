{-# LANGUAGE FlexibleInstances #-}
module Text.Pinpoint ( Pinpoint ) where
import Text.Pin
import Text.Point
import {-# SOURCE #-} qualified Data.Directory as Dir
import Control.Applicative hiding ( (<|>) )
import Control.DateTime.Moment
-- TODO: relax the constraint on Dir.location ; import Control.Monad.Trans
import Text.Fragment
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.TagWiki

data Pinpoint = One Pin | Both Pin Point deriving Eq

instance Show Pinpoint where
    show (One pin) = show pin
    show (Both pin point) = show pin ++ " " ++ show point

instance Parseable Pinpoint where
    parser = try (Both <$> parser <*> (whitespace >> parser))
         <|> (One <$> parser)
         <?> "pinpoint"

instance Fragment Pinpoint where
    resolve (One pin) = Dir.location pin Nothing
    resolve (Both pin point) = Dir.location pin (Just point)

instance Momentus Pinpoint where
    moment (One pin) = Dir.pinpoint pin Nothing
    moment (Both pin point) = Dir.pinpoint pin (Just point)
