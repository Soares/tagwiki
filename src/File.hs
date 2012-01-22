{-# LANGUAGE ExistentialQuantification #-}
module File where
import Control.Dangerous
import Control.Monad.Reader
import Text.DateTime.Moment
import {-# SOURCE #-} Database
import {-# SOURCE #-} Text.Event ( Event )

data File = forall f . Fileish f => File f

class (Show f) => Fileish f where
    pinpoint :: [String] -> f -> DangerousT (Reader Database) Moment
    reference :: [String] -> f -> String

instance Fileish File where
    pinpoint xs (File f) = pinpoint xs f
    reference xs (File f) = reference xs f
instance Show File where
    show (File f) = show f
