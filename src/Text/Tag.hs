module Text.Tag ( tag ) where
import Control.Applicative hiding ( optional )
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.TagWiki
import qualified Text.Symbols as Y

tag :: GenParser Char st String
tag = except Y.restrictedInRefs <* optional halt where
    halt = whitespace >> string Y.halt >> whitespace >> pure ()
