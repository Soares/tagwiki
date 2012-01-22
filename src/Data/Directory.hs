module Data.Directory where
import Control.Monad.Reader
import Control.Dangerous
import Control.DateTime.Offset
import Data.File

data Directory = Dir

type Operation = DangerousT (Reader Directory)

era :: String -> Reader Directory [Offset]
era _ = return []

file :: [String] -> [String] -> String -> Reader Directory (Maybe File)
file _ _ _ = return Nothing
-- data Note = Note
-- instance File Note where names = const []

-- data Character = Character
-- instance File Character where names = const []

-- find :: (File f) => String -> [String] -> [String] -> f
-- find [] cats qals = Note
-- find _ cats quals = Character
