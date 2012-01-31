module Text.Render where
import Text.Printf
import Text.Utils

header :: String -> String
header = printf "<h1>%s</h1>"

title :: String -> String
title = printf "%% %s\n"

reference :: [(String, String)] -> String
reference = printf "<div id='reference'>\n%s\n</div>" . concatMap lnk where
    lnk :: (String, String) -> String
    lnk = uncurry $ printf "<a href='#%s'>%s</a>\n"

list :: [String] -> String
list = printf "<ul>\n%s\n</ul>" . concatMap li where
    li :: String -> String
    li = printf "<li>%s</li>\n"

section :: String -> String -> String
section name = printf "<section id='%s'>\n%s\n%s\n</section>\n"
    (slugify name) (header name)

article :: String -> String -> String
article name = printf "<article id='%s'>\n<h2>%s</h2>\n%s</article>\n"
    (slugify name) name

-- TODO: "id='a-href-id-some-id-string-a" HEH
link :: String -> String -> String
link = printf "<a href='%s'>%s</a>"

href :: Maybe String -> String -> String
href Nothing ident = ident
href (Just x) ident = printf "%s#%s" ident x
