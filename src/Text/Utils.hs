module Text.Utils where
import Data.Char ( toLower )
import Text.Regex

normalize :: String -> String
normalize x = normWhite $ map toLower x where
    normWhite str = subRegex manyWhite str " "
    manyWhite = mkRegex "[\t\n ]+"


like :: String -> String -> Bool
x `like` y = normalize x == normalize y
