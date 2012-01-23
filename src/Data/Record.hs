module Data.Record where
import Data.Head

class Record where
    matches :: String -> Head -> Maybe Bool
    name :: Head -> String
    tags :: Head -> [Sting]
