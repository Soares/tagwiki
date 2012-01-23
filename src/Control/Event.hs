{-# LANGUAGE FlexibleInstances #-}
module Control.Event
    ( Event(..)
    , pinpoint
    , isStartEvent
    , isEndEvent
    , isDurationEvent ) where
import Control.Applicative ( (<$>), (<*>) )
import Control.Monad
import Control.Dangerous hiding ( Warning )
import Data.Char ( toLower )
import Data.List
import Data.String.Utils
import Data.Directory
import Control.DateTime.Calculation
import Control.DateTime.Expression
import Control.DateTime.Moment
import Text.Fragment
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.TagWiki
import Text.Printf
import Control.Reference ( tag )
import Text.Render
import Control.Unit ( Unit, block )
import qualified Text.Symbols as Y

data Event = Event { name :: String
                   , date :: Calculation
                   , text :: [Unit]
                   } deriving Eq

-- Reducing to moment
pinpoint :: [String] -> Event -> Operation Moment
pinpoint [] (Event _ w _) = beginning w
pinpoint (x:xs) (Event _ w _) = do
    unless (null xs) (warn $ Ignored xs)
    let start = isStartEvent x
    when (not start && not (isEndEvent x)) (warn $ Unrecognized x)
    if start then beginning w else ending w

isStartEvent :: String -> Bool
isStartEvent x = (map toLower . strip) x `elem`
    [ "first"
    , "begin"
    , "beginning"
    , "birth"
    , "dawn"
    , "born"
    , "creation"
    , "created"
    , "start" ]

isDurationEvent :: String -> Bool
isDurationEvent x = (map toLower . strip) x `elem`
    [ "alive"
    , "duration"
    , "life"
    , "timeframe" ]

isEndEvent :: String -> Bool
isEndEvent x = (map toLower . strip) x `elem`
    [ "death"
    , "died"
    , "destruction"
    , "destroyed"
    , "end"
    , "ending"
    , "fall"
    , "last" ]



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
