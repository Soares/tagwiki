module Data.Cache where
import Data.Map ( Map )
import Text.Pinpoint
import qualified Data.Map as Map
import {-# SOURCE #-} Control.DateTime.Moment

data Cache = Cache { moments   :: Map Pinpoint Moment
                   , locations :: Map Pinpoint String
                   , offsets   :: Map String (Maybe (Direction, Moment))
                   } deriving Show

putMoment :: Pinpoint -> Moment -> Cache -> Cache
putMoment k a c = c{moments=Map.insert k a $ moments c}

getMoment :: Pinpoint -> Cache -> Maybe Moment
getMoment k = Map.lookup k . moments

putLocation :: Pinpoint -> String -> Cache -> Cache
putLocation k a c = c{locations=Map.insert k a $ locations c}

getLocation :: Pinpoint -> Cache -> Maybe String
getLocation k = Map.lookup k . locations

putOffset :: String -> Maybe (Direction, Moment) -> Cache -> Cache
putOffset k a c = c{offsets=Map.insert k a $ offsets c}

getOffset :: String -> Cache -> Maybe (Maybe (Direction, Moment))
getOffset k = Map.lookup k . offsets

empty :: Cache
empty = Cache Map.empty Map.empty Map.empty
