{-# LANGUAGE FlexibleInstances #-}
module Text.Pinpoint ( Pinpoint(..), pin, point, isSelf, setPin ) where
import Text.Pin
import Text.Point
import {-# SOURCE #-} qualified Data.Directory as Dir
import Control.Applicative hiding ( (<|>), empty )
import Control.DateTime.Moment
-- TODO: remove Pin.hs-boot and Point.hs-boot
import Text.Fragment
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.TagWiki

data Pinpoint = One Pin | Both Pin Point deriving (Eq, Ord)

pin :: Pinpoint -> Pin
pin (One p) = p
pin (Both p _) = p

setPin :: Pin -> Pinpoint -> Pinpoint
setPin p (One _) = One p
setPin p (Both _ pt) = Both p pt

point :: Pinpoint -> Maybe Point
point (One _) = Nothing
point (Both _ p) = Just p

isSelf :: Pinpoint -> Bool
isSelf = (== empty) . pin

instance Show Pinpoint where
    show (One p) = show p
    show (Both p t) = show p ++ show t

instance Parseable Pinpoint where
    parser = try (Both <$> parser <*> (whitespace >> parser))
         <|> try (Both empty <$> (whitespace >> parser))
         <|> (One <$> parser)
         <?> "pinpoint"

instance Fragment Pinpoint where resolve = Dir.location
instance Momentus Pinpoint where moment = Dir.pinpoint
