module Data.Cache where
import {-# SOURCE #-} Control.DateTime.Moment
import Text.Pinpoint
import Data.Map ( Map )
import qualified Data.Map as Map

data Cache = Cache { moments   :: Map Pinpoint Moment
                   , locations :: Map Pinpoint String
                   , offsets   :: Map String Moment
                   }

recordMoment :: Pinpoint -> Moment -> Cache -> Cache
recordMoment k a c = c{moments=Map.insert k a $ moments c}

recordLocation :: Pinpoint -> String -> Cache -> Cache
recordLocation k a c = c{locations=Map.insert k a $ locations c}

recordOffset :: String -> Moment -> Cache -> Cache
recordOffset k a c = c{offsets=Map.insert k a $ offsets c}
