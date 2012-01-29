module Data.File where
import {-# SOURCE #-} Data.Record ( Record )

data File
instance Show File
instance Eq File
instance Ord File
instance Record File
