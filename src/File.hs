{-# LANGUAGE ExistentialQuantification #-}
module File where
import {-# SOURCE #-} Text.Event

data File = forall f . Fileish f => File f

class (Show f) => Fileish f where
    pinpoint :: f -> [String] -> Maybe Event
    reference :: f -> [String] -> String

instance Fileish File where
    pinpoint (File f) = pinpoint f
    reference (File f) = reference f
instance Show File where
    show (File f) = show f
