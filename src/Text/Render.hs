module Text.Render where
import Data.Char ( toLower )
import Data.List
import Data.String.Utils
import Text.Printf

section :: String -> String -> String
section name = printf "<section id='%s'><h1>%s</h1>%s</section>"
    (slugify name) name

article :: String -> String -> String
article name = printf "<article id='%s'><h2>%s</h2>%s</article>"
    (slugify name) name

link :: String -> String -> String
link = printf "<a href='%s'>%s</a>"

slugify :: String -> String
slugify = replace "\t" "-" . replace " " "-" . map toLower

document :: String -> String -> String -> String
document = printf "%% %s\n%s\n%s"

href :: String -> [String] -> String
href ident events = printf "%s-%s" ident (intercalate "_" events)
