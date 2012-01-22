module Text.DateTime.AbsDate where
import Data.Functor
import Text.DateTime.Parser
import Text.DateTime.Year
import Text.ParserCombinators.TagWiki
import Text.Printf
import Text.DateTime.Utils

data AbsDate = AbsDate { year  :: Year
                       , month :: Maybe Int
                       , day   :: Maybe Int
                       } deriving Eq

instance Show AbsDate where
    show (AbsDate y m d) = printf "%s/%s/%s"
        (show y) (intOrBlank m) (intOrBlank d)

instance Parseable AbsDate where
    parser = getYear >>= getMonth >>= getDay where
        getYear = (\y -> AbsDate y Nothing Nothing) <$> parser
        getMonth date = (\m -> date{month=m}) <$> markedInt slash
        getDay date = (\d -> date{day=d}) <$> markedInt slash
