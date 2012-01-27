module Data.Directory
    ( Directory
    , Operation
    , eraOffset
    , eraOffsets
    , pinpoint
    , location
    ) where
import Control.DateTime.Offset
import Control.Monad.Reader
import Control.Dangerous
import {-# SOURCE #-} Control.DateTime.Moment
import {-# SOURCE #-} Text.Pin
import {-# SOURCE #-} Text.Point

data Directory
type Operation = ReaderT Directory Dangerous

eraOffset :: String -> Operation (Maybe Offset)
eraOffsets :: String -> Operation [Offset]
pinpoint :: Pin -> Maybe Point -> Operation Moment
location :: Pin -> Maybe Point -> Operation String
