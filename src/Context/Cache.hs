module Context.Cache where
import Context.Reference
import Data.File
import Data.Map ( Map )
import Text.Pin
import qualified Data.Map as Map
import Control.DateTime.Moment

data Cache = Cache
    { refs  :: Map Reference Moment
    , pins  :: Map Pin (Maybe File)
    , codes :: Map String (Maybe Offset)
    } deriving Show

putRef :: Reference -> Moment -> Cache -> Cache
putRef k a c = c{refs=Map.insert k a $ refs c}

getRef :: Reference -> Cache -> Maybe Moment
getRef k = Map.lookup k . refs

putPin :: Pin -> Maybe File -> Cache -> Cache
putPin k a c = c{pins=Map.insert k a $ pins c}

getPin :: Pin -> Cache -> Maybe (Maybe File)
getPin k = Map.lookup k . pins

putCode :: String -> Maybe Offset -> Cache -> Cache
putCode k a c = c{codes=Map.insert k a $ codes c}

getCode :: String -> Cache -> Maybe (Maybe Offset)
getCode k = Map.lookup k . codes

empty :: Cache
empty = Cache Map.empty Map.empty Map.empty
