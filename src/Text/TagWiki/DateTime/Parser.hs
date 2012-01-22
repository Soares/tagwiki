module Text.TagWiki.DateTime.Parser where
import Text.Parser
import Text.ParserCombinators.Parsec
import qualified Text.TagWiki.Symbols as Y

slash, colin, dot :: GenParser Char st ()
slash = operator Y.dateSep
colin = operator Y.minSep
dot = operator Y.secSep
