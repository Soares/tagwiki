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
import {-# SOURCE #-} Control.Reference

data Directory
type Operation = DangerousT (Reader Directory)

eraOffset :: String -> Operation (Maybe Offset)
eraOffsets :: String -> Operation [Offset]
pinpoint :: Reference -> Operation Moment
location :: Reference -> Operation String
