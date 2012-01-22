module Control.DateTime.AbsDate ( AbsDate ) where
import Data.Functor
import Control.DateTime.Era
import Control.DateTime.Moment
import Control.DateTime.Parser
import Text.ParserCombinators.TagWiki
import Text.Printf
import Control.DateTime.Utils

data AbsDate = Abs { year  :: Int
                   , month :: Maybe Int
                   , day   :: Maybe Int
                   , era   :: Era
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
                              , era   = e}) <$> parser
