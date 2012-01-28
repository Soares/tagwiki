module Text.Fragment where
import {-# SOURCE #-} Data.Directory

class Fragment a where
    resolve :: (Momentable m) => a -> m String
