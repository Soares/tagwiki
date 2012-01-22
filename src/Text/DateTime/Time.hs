module Text.DateTime.Time ( Time ) where
import Control.Applicative ( (<*) )
import Data.Functor
import Text.DateTime.Moment
import Text.ParserCombinators.TagWiki
import Text.Printf
import Text.DateTime.Parser
import Text.DateTime.Utils


data Time = Time { hour   :: Maybe Int
                 , minute :: Maybe Int
                 , second :: Maybe Int
                 , detail :: Maybe Int
                 } deriving Eq

instance Dateable Time where
    date (Time h m s d) = return $ Known (dateParts ++ [h, m, s, d]) Nothing
        where dateParts = [Nothing, Nothing, Nothing]

instance Show Time where
    show (Time h m s d) = printf "%s:%s.%s.%s"
        (intOrBlank h) (intOrBlank m) (intOrBlank s) (intOrBlank d)

instance Parseable Time where
    parser = getHour >>= getMinute >>= getSecond >>= getDetail where
        getHour        = (\h -> noclock{hour=h}) <$> maybeInt <* colin
        getMinute time = (\m -> time{minute=m }) <$> maybeInt
        getSecond time = (\s -> time{second=s }) <$> markedInt dot
        getDetail time = (\d -> time{detail=d }) <$> markedInt dot


noclock :: Time
noclock = Time Nothing Nothing Nothing Nothing
