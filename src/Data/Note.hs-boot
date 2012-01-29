module Data.Note where
import Text.Pin
import Data.Body
data Note = Note { names      :: [(Bool, String)]
                 , tags       :: [String]
                 , categories :: [String]
                 , qualifiers :: [String]
                 , body       :: Body
                 }

pin :: String -> Note -> Pin
