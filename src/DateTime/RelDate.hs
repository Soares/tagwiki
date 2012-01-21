module DateTime.RelDate where
import Control.Applicative ( (<*) )
import Data.Functor
import DateTime.Parsing
import Text.Printf
import Parsing
import Utils

data RelDate = RelDate { year  :: Maybe Int
                       , month :: Maybe Int
                       , day   :: Maybe Int
                       } deriving Eq

instance Show RelDate where
    show (RelDate y m d) = printf "%s/%s/%s"
        (intOrBlank y) (intOrBlank m) (intOrBlank d)

instance Parseable RelDate where
    parser = getYear >>= getMonth >>= getDay where
        getYear = (\y -> whenever{year=y}) <$> maybeInt <* slash
        getMonth date = (\m -> date{month=m}) <$> maybeInt
        getDay date = (\d -> date{day=d}) <$> markedInt slash

fromYear :: Int -> RelDate
fromYear y = whenever{year = Just y}

whenever :: RelDate
whenever = RelDate Nothing Nothing Nothing
