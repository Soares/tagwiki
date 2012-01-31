-- {-# LANGUAGE FlexibleContexts #-}
-- {-# LANGUAGE FlexibleInstances #-}
module Internal where
import Control.Applicative
import Control.Dangerous
import Text.Pin
import Text.Point
import Text.Pinpoint
import {-# SOURCE #-} Data.File
import {-# SOURCE #-} Control.DateTime.Moment

class (Applicative i, Errorable i) => Internal i where
    lookupEraCode :: String -> i (Maybe Offset)
    doWithEra :: String -> i a -> i a
    pinpoint :: Pinpoint -> i Moment
    doWithRef :: File -> Maybe Point -> i a -> i a
    location :: Pinpoint -> i String
    find :: Pin -> i (Maybe File)
    build :: (FilePath -> File -> i a) -> i [a]
