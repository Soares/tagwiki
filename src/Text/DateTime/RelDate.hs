module Text.DateTime.RelDate ( RelDate, fromYear, whenever ) where
import Control.Applicative ( (<*) )
import Data.Functor
import Text.ParserCombinators.TagWiki
import Text.Printf
import Text.DateTime.Moment
import Text.DateTime.Parser
import Text.DateTime.Utils

data RelDate = Rel { year  :: Maybe Int
                   , month :: Maybe Int
                   , day   :: Maybe Int
                   } deriving Eq

instance Dateable RelDate where
    date (Rel y m d) = return $ Known [y, m, d] Nothing

instance Show RelDate where
    show (Rel y m d) = printf "%s/%s/%s"
        (intOrBlank y) (intOrBlank m) (intOrBlank d)

instance Parseable RelDate where
    parser = getYear >>= getMonth >>= getDay where
        getYear = (\y -> whenever{year=y}) <$> maybeInt <* slash
        getMonth rel = (\m -> rel{month=m}) <$> maybeInt
        getDay rel = (\d -> rel{day=d}) <$> markedInt slash

fromYear :: Int -> RelDate
fromYear y = whenever{year = Just y}

whenever :: RelDate
whenever = Rel Nothing Nothing Nothing
