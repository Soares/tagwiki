module DateTime.Time where
import Control.Applicative ( (<*) )
import Data.Functor
import DateTime.Parsing
import Text.Printf
import Parsing
import Utils


data Time = Time { hour   :: Maybe Int
                 , minute :: Maybe Int
                 , second :: Maybe Int
                 , detail :: Maybe Int
                 } deriving Eq


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
