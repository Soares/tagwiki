module Utils where
import Data.Maybe

intOrBlank :: Maybe Int -> String
intOrBlank = fromMaybe "" . fmap show
