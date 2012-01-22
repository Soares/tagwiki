module Control.Event ( Event, pinpoint ) where
import Control.DateTime.Moment
import {-# SOURCE #-} Data.Directory

data Event

pinpoint :: [String] -> Event -> Operation Moment
