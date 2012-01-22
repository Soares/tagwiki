module Database where
import Control.Monad.Reader
import Control.Dangerous
import File
import Text.DateTime.Offset

data Database = Database

type Operation = DangerousT (Reader Database)

era :: String -> Reader Database [Offset]
era _ = return []

file :: [String] -> [String] -> String -> Reader Database (Maybe File)
file _ _ _ = return Nothing
-- data Note = Note
-- instance File Note where names = const []

-- data Character = Character
-- instance File Character where names = const []

-- find :: (File f) => String -> [String] -> [String] -> f
-- find [] cats qals = Note
-- find _ cats quals = Character
