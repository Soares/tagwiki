{-# LANGUAGE FlexibleInstances #-}
module Control.Event ( Event(..), recognizes, at ) where
import Internal
import Control.Applicative hiding ( (<|>) )
import Control.DateTime.Calculation ( Calculation(..) )
import Control.DateTime.Expression
import Control.DateTime.Moment
import Control.Unit ( Unit, block )
import Data.String.Utils
import Text.Fragment
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.TagWiki
import Text.Point ( Side )
import Text.Printf
import Text.Render
import Text.Utils
import qualified Text.Tag as Tag
import qualified Text.Symbols as Y

data Event = Event
    { name :: String
    , when :: Maybe Calculation
    , text :: [Unit]
    } deriving Eq

tag :: Event -> String
tag = normalize . name


-- | Reducing to moment
recognizes :: String -> Event -> Bool
recognizes "" = const True
recognizes pt = (pt ==) . tag

at :: (Internal i) => Side -> Event -> i (Maybe Moment)
at side ev = case when ev of
    Just calc -> Just <$> moment (side, calc)
    Nothing -> pure Nothing


-- | Reducing to text
instance Fragment Event where
    resolve (Event n Nothing t) = article h <$> resolve t
        where h = printf "%s (?)" (strip n)
    resolve (Event n (Just w) t) = article <$> h <*> resolve t
        where h = printf "%s %s" (strip n) <$> resolve w



-- | Parsing
instance Parseable Event where
    parser = marker Y.event >> (Event <$> Tag.tag <*> calc <*> block) where
        calc = try (Just <$> parser)
           <|> try (Just . Exactly <$> (DateTime <$> parser))
           <|> pure Nothing



-- Showing
instance Show Event where
    show (Event k w _) = printf "!%s @%s" (show k) (show w)
