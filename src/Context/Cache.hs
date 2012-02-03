module Context.Cache where
import Context.Reference
import Control.DateTime.Absolute
import Data.File
import Data.Map ( Map )
import Internal
import Text.Pin
import qualified Data.Map as Map

data Cache = Cache
    { refs  :: Map Reference Absolute
    , pins  :: Map Pin (Maybe File)
    , codes :: Map String (Maybe (Direction, Absolute))
    } deriving Show

putRef :: Reference -> Absolute -> Cache -> Cache
putRef k a c = c{refs=Map.insert k a $ refs c}

getRef :: Reference -> Cache -> Maybe Absolute
getRef k = Map.lookup k . refs

putPin :: Pin -> Maybe File -> Cache -> Cache
putPin k a c = c{pins=Map.insert k a $ pins c}

getPin :: Pin -> Cache -> Maybe (Maybe File)
getPin k = Map.lookup k . pins

putCode :: String -> Maybe (Direction, Absolute) -> Cache -> Cache
putCode k a c = c{codes=Map.insert k a $ codes c}

getCode :: String -> Cache -> Maybe (Maybe (Direction, Absolute))
getCode k = Map.lookup k . codes

empty :: Cache
empty = Cache Map.empty Map.empty Map.empty
