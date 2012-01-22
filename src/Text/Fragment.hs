module Text.Fragment where
import Data.Functor
import {-# SOURCE #-} Data.Directory

class Fragment a where
    resolve :: a -> Operation String

instance (Fragment a) => Fragment [a] where
    resolve as = concat <$> mapM resolve as
