module Control.DateTime.Moment ( Moment, Direction(..) ) where
data Moment
instance Show Moment
data Direction = Positive | Negative
instance Show Direction
