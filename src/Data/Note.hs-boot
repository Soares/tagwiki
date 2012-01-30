module Data.Note where
import Text.Pin
import Data.Body
import Text.Pinpoint
data Note = Note
    { source     :: FilePath
    , categories :: [String]
    , qualifiers :: [Pinpoint]
    , body       :: Body
    , names      :: [(Bool, String)]
    , tags       :: [String]
    }

pin :: String -> Note -> Pin
