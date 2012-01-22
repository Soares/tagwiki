{-# OPTIONS_GHC -XFlexibleInstances #-}
module Text.Event ( Event(..) ) where
import Control.Monad
import Data.Functor
import Data.String.Utils
import Text.DateTime.Calculation
import Text.DateTime.Expression
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

instance Show Event where
    show (Event k w _) = printf "!%s @%s" (show k) (show w)

instance Parseable Event where
    parser = marker Y.event >> liftM3 Event tag date block

instance Fragment [Event] where
    resolve db xs = section "events" $ concatMap (resolve db) xs

instance Fragment Event where
    resolve db (Event n w t) = article h (resolve db w)
        where h = printf "%s (%s)" (strip n) (resolve db t)

date :: GenParser Char st Calculation
date = try parser
   <|> try (Exactly <$> liftM2 Clobber (Abs <$> parser) (At <$> parser))
   <|> (Exactly <$> (Abs <$> parser))
   <?> "date for event"
