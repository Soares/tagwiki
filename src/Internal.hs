module Internal where
import Control.Applicative
import Control.Dangerous
import Control.DateTime.Absolute
import Text.Pin
import Text.Point
import Text.Pinpoint
import {-# SOURCE #-} Data.File

class (Applicative i, Errorable i) => Internal i where
    pinpoint :: Pinpoint -> i Absolute
    location :: Pinpoint -> i String
    find :: Pin -> i (Maybe File)
    build :: (FilePath -> File -> i a) -> i [a]

class Momented m where
    when :: (Internal i) => Side -> m -> i Absolute

data Direction
    = Before
    | After
    deriving (Eq, Ord, Read, Show)
