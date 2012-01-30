module Timeline where

data Timeline = Timeline
    { id            :: String
    , title         :: String
    , description   :: Maybe String
    , focus_date    :: Maybe Moment
    , initialZoom   :: Maybe Int
    , events        :: [Event]
    , legend        :: [Legend]
    }

data Legend = Legend
    { title         :: String
    , icon          :: URL
    }

data Event = Event
    { id            :: String
    , title         :: String
    , description   :: Maybe String
    , startDate     :: Moment
    , endDate       :: Maybe Moment
    , dateDisplay   :: Maybe String
    , link          :: URL
    , icon          :: Maybe URL
    , importance    :: Int
    -- Map not supported
    }

timelines :: Directory -> [Timeline]
