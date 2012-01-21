module DateTime.Parsing where
import Parsing
import Text.ParserCombinators.Parsec
import qualified Symbols as Y

slash, colin, dot :: GenParser Char st ()
slash = operator Y.dateSep
colin = operator Y.minSep
dot = operator Y.secSep
