module Text.Utils where
import Data.Char ( toLower )
import Data.String.Utils ( replace )
import Text.Regex

slugify :: String -> String
slugify = replace " " "-" . normalize

normalize :: String -> String
normalize x = normWhite $ map toLower x where
    normWhite str = subRegex manyWhite str " "
    manyWhite = mkRegex "[\t\n ]+"


like :: String -> String -> Bool
x `like` y = normalize x == normalize y
