{-# LANGUAGE ExistentialQuantification #-}
module Data.File where
import Control.Dangerous
import Control.Monad.Reader
import Control.DateTime.Moment
import {-# SOURCE #-} Data.Directory
import {-# SOURCE #-} Control.Event ( Event )

data File = forall f . Fileish f => File f

class (Show f) => Fileish f where
    pinpoint :: [String] -> f -> DangerousT (Reader Directory) Moment
    reference :: [String] -> f -> String

instance Fileish File where
    pinpoint xs (File f) = pinpoint xs f
    reference xs (File f) = reference xs f
instance Show File where
    show (File f) = show f
