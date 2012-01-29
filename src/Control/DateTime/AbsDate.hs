module Control.DateTime.AbsDate ( AbsDate ) where
import Control.DateTime.Moment
import Control.DateTime.Parser
import Control.DateTime.Utils
import Data.Functor
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.TagWiki
import Text.Printf

data AbsDate = Abs { year  :: Int
                   , month :: Maybe Int
                   , day   :: Maybe Int
                   , era   :: String
                   } deriving Eq

instance Dateable AbsDate where
    date (Abs y m d e) = return $ Known [Just y, m, d] (Just e)

instance Show AbsDate where
    show (Abs y m d e) = printf "%s%s/%s/%s"
        (show y) (show e) (intOrBlank m) (intOrBlank d)

instance Parseable AbsDate where
    parser = number >>= getEra >>= getMonth >>= getDay where
        getMonth ab = (\m -> ab{month=m}) <$> markedInt slash
        getDay ab = (\d -> ab{day=d}) <$> markedInt slash
        getEra y = (\e -> Abs { year  = y
                              , month = Nothing
                              , day   = Nothing
                              , era   = e}) <$> many1 letter
