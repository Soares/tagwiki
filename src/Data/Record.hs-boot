module Data.Record where
import Text.Fragment
import {-# SOURCE #-} Data.Head
import {-# SOURCE #-} Data.Body

class Record where
    matches :: String -> Head -> Maybe Bool
    name :: Head -> String
    tags :: Head -> [String]
