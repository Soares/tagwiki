module Text.Event ( Event, pinpoint ) where
import Text.DateTime.Moment
import {-# SOURCE #-} Database

data Event

pinpoint :: [String] -> Event -> Operation Moment
