module Text.Point ( Point(..) , Side(Start, End) ) where
import Text.ParserCombinators.TagWiki
data Side = Start | End
data Point = Point { side :: Side, name :: String }
instance Eq Point
instance Show Point
instance Parseable Point
