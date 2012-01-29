module Data.Note where
import Text.Pin
import Data.Body
import Text.Pinpoint
data Note = Note { source     :: FilePath
                 , names      :: [(Bool, String)]
                 , tags       :: [String]
                 , categories :: [String]
                 , qualifiers :: [Pinpoint]
                 , body       :: Body
                 }

pin :: String -> Note -> Pin
