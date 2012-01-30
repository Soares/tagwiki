module Text.Tag ( tag, cleanTag ) where
import Control.Applicative hiding ( optional )
import Data.String.Utils ( strip )
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.TagWiki
import qualified Text.Symbols as Y

tag :: GenParser Char st String
tag = except Y.restrictedInRefs <* optional halt where
    halt = whitespace >> string Y.halt >> whitespace >> pure ()

cleanTag :: GenParser Char st String
cleanTag = strip <$> tag
