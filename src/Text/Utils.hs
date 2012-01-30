module Text.Utils where
import Data.Char ( toLower )
import Data.String.Utils ( replace, strip )
import Text.Regex

slugify :: String -> String
slugify = replace " " "-" . normalize

normalize :: String -> String
normalize x = strip $ normWhite $ map toLower x where
    normWhite str = subRegex manyWhite str " "
    manyWhite = mkRegex "[\t\n ]+"
