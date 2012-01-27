{-# LANGUAGE FlexibleInstances #-}
module Control.Event ( Event(..), recognizes ) where
import Control.Applicative ( (<$>), (<*>) )
import Control.DateTime.Calculation
import Control.DateTime.Expression
import Control.Monad hiding ( when )
import Control.Unit ( Unit, block )
import Data.List
import Data.String.Utils
import Text.Fragment
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.TagWiki
import Text.Point ( Point )
import Text.Printf
import Text.Render
import Text.Utils
import Text.Tag ( tag )
import qualified Text.Point as Point
import qualified Text.Symbols as Y

data Event = Event { name :: String
                   , when :: Calculation
                   , text :: [Unit]
                   } deriving Eq

-- Reducing to moment
recognizes :: Point -> Event -> Bool
recognizes p v = name v `like` Point.name p


-- Reducing to text
instance Fragment Event where
    resolve (Event n w t) = article <$> h <*> resolve t
        where h = printf "%s (%s)" (strip n) <$> resolve w



-- Parsing
instance Parseable Event where
    parser = marker Y.event >> liftM3 Event tag calc block where
        datePart = Abs <$> parser
        timePart = At <$> parser
        calc = try parser
           <|> try (Exactly <$> (Clobber <$> datePart <*> timePart))
           <|> (Exactly <$> datePart)
           <?> "date for event"



-- Showing
instance Show Event where
    show (Event k w _) = printf "!%s @%s" (show k) (show w)



--- Warnings
data Warning = Ignored [String]
             | Unrecognized String
instance Show Warning where
    show (Ignored xs) = printf
        "Extra events ignored: [%s]" (intercalate ", " xs)
    show (Unrecognized x) = printf
        "Unrecognized event (resolved as !start) %s" x
