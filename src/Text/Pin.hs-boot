module Text.Pin ( Pin(..), empty ) where
import Text.ParserCombinators.TagWiki
data Pin = Pin { tag        :: String
               , categories :: [String]
               , qualifiers :: [String] }
instance Eq Pin
instance Show Pin
instance Parseable Pin
empty :: Pin
