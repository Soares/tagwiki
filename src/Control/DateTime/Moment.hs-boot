module Control.DateTime.Moment ( Moment, Direction(..), Offset(..) ) where
data Moment
data Direction = Positive | Negative
data Offset = Descending Direction Moment | Root
instance Show Moment
instance Show Direction
instance Show Offset
