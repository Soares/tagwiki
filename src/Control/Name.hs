module Control.Name where
import Control.Applicative
import Text.Tag ( tag )
import Text.ParserCombinators.TagWiki
import Text.ParserCombinators.Parsec ( option, string )
import qualified Text.Symbols as Y

data Priority = Pri Int deriving (Eq, Ord)

high, low :: Priority
high = Pri 0
low = Pri 10

priorities :: [Priority]
priorities = [low, high]

data Name = Name
    { priority :: Priority
    , namePart :: String
    } deriving (Eq, Ord)

ofPriority :: Priority -> [Name] -> [String]
ofPriority p = map namePart . filter ((== p) . priority)

instance Parseable Priority where
    parser = option low (string Y.priority *> pure high)

instance Parseable Name where
    parser = whitespace *> (Name <$> parser <*> str) where
        str = (++) <$> option "" escPri <*> tag
        escPri = hack *> string Y.priority
