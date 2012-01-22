module Text.DateTime.Parser where
import Text.ParserCombinators.TagWiki
import Text.ParserCombinators.Parsec
import qualified Text.Symbols as Y

slash, colin, dot :: GenParser Char st ()
slash = operator Y.dateSep
colin = operator Y.minSep
dot = operator Y.secSep
