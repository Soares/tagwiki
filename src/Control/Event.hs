{-# LANGUAGE FlexibleInstances #-}
module Control.Event ( Event(..), recognizes ) where
import Internal
import Control.Applicative hiding ( (<|>) )
import Control.Dangerous
import Control.DateTime.Calculation ( Calculation(..) )
import Control.DateTime.Absolute hiding ( normalize )
import Control.Unit ( Unit, block )
import Data.String.Utils
import Text.Fragment
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.TagWiki
import Text.Printf
import Text.Render
import Text.Utils
import qualified Control.DateTime.Calculation as Calc
import qualified Text.Tag as Tag
import qualified Text.Symbols as Y

data Event = Event
    { name :: String
    , at   :: Maybe Calculation
    , text :: [Unit]
    }

tag :: Event -> String
tag = normalize . name


-- | Reducing to moment
recognizes :: String -> Event -> Bool
recognizes "" = const True
recognizes pt = (pt ==) . tag

instance Momented Event where
    when side ev = case at ev of
        Just calc -> when side calc
        Nothing -> warn (UnknownEvent ev) *> pure Present


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
           <|> try (Just . Exactly <$> parser)
           <|> pure Nothing



-- | Showing
instance Show Event where
    show (Event k w _) = printf "!%s @%s" (show k) (show w)


-- | Warnings
data Unknown = UnknownEvent Event
instance Show Unknown where
    show (UnknownEvent ev) = printf "Unknown event: %s" (show ev)
