module Text.Render where
import Data.Char ( toLower )
import Data.String.Utils
import Text.Printf

section :: String -> String -> String
section name body = printf "<section id='%s'><h1>%s</h1>%s</section>"
    (slugify name) name body

article :: String -> String -> String
article name body = printf "<article id='%s'><h2>%s</h2>%s</article>"
    (slugify name) name body

link :: String -> String -> String
link text href = printf "<a href='%s'>%s</a>" href text

slugify :: String -> String
slugify = replace "\t" "-" . replace " " "-" . map toLower
