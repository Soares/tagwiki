module Text.Utils where
import Data.Char ( toLower )
import Data.String.Utils ( strip )
import Text.Regex

slugify :: String -> String
slugify str = subRegex invalid (normalize str) "-" where
    invalid = mkRegex "[^-_0-9a-zA-Z]+"

normalize :: String -> String
normalize x = strip $ normWhite $ map toLower x where
    normWhite str = subRegex manyWhite str " "
    manyWhite = mkRegex "[\t\n ]+"
