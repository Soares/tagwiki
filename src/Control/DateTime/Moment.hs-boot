module Control.DateTime.Moment ( Moment, Direction(..), Offset(..) ) where
data Moment
data Direction = Before | After
data Offset = Descending Direction Moment | Root
instance Show Moment
instance Show Offset
