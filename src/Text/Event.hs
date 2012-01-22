{-# LANGUAGE FlexibleInstances #-}
module Text.Event ( Event(..) ) where
import Control.Applicative ( (<$>), (<*>) )
import Control.Monad
import Data.String.Utils
import Text.DateTime.Calculation
import Text.DateTime.Expression
import Text.DateTime.Moment
import Text.Fragment
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.TagWiki
import Text.Printf
import Text.Reference ( tag )
import Text.Render
import Text.Unit ( Unit, block )
import qualified Text.Symbols as Y

data Event = Event { name :: String
                   , when :: Calculation
                   , text :: [Unit]
                   } deriving Eq

instance Dateable Event where
    date (Event _ w _) = date w

instance Show Event where
    show (Event k w _) = printf "!%s @%s" (show k) (show w)

instance Parseable Event where
    parser = marker Y.event >> liftM3 Event tag calc block

instance Fragment [Event] where
    resolve xs = section "events" . concat <$> mapM resolve xs

instance Fragment Event where
    resolve (Event n w t) = article <$> h <*> resolve t
        where h = printf "%s (%s)" (strip n) <$> resolve w

calc :: GenParser Char st Calculation
calc = try parser
   <|> try (Exactly <$> (Clobber <$> (Abs <$> parser) <*> (At <$> parser)))
   <|> (Exactly <$> (Abs <$> parser))
   <?> "date for event"
