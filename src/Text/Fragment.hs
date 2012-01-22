module Text.Fragment where
import Control.Monad.Reader
import Data.Functor
import {-# SOURCE #-} Database

class Fragment a where
    resolve :: a -> Reader Database String

instance (Fragment a) => Fragment [a] where
    resolve as = concat <$> mapM resolve as
