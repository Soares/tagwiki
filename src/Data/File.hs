{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
module Data.File where
import Control.Dangerous
import Control.Monad.Reader
import Control.DateTime.Moment
import Text.Fragment
import {-# SOURCE #-} Data.Directory

data File = forall f . Record f => File f

class (Fragment f) => Record f where
    pinpoint :: [String] -> f -> DangerousT (Reader Directory) Moment
    reference :: [String] -> f -> String
    title :: f -> String

instance Record File where
    pinpoint xs (File f) = pinpoint xs f
    reference xs (File f) = reference xs f
    title (File f) = title f

instance Fragment File where
    resolve (File f) = resolve f

instance Show File where
    show (File f) = title f
