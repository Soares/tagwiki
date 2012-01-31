module Context.Cache where
import Data.File
import Data.Map ( Map )
import Text.Pin
import Text.Pinpoint
import qualified Data.Map as Map
import Control.DateTime.Moment

data Cache = Cache
    { pinpoints :: Map Pinpoint Moment
    , pins      :: Map Pin (Maybe File)
    , codes     :: Map String (Maybe Offset)
    } deriving Show

putPinpoint :: Pinpoint -> Moment -> Cache -> Cache
putPinpoint k a c = c{pinpoints=Map.insert k a $ pinpoints c}

getPinpoint :: Pinpoint -> Cache -> Maybe Moment
getPinpoint k = Map.lookup k . pinpoints

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
