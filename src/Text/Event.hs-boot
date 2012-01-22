module Text.Event ( Event ) where
import Text.DateTime.Moment

data Event
instance Dateable Event
