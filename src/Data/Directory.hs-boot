module Data.Directory ( Directory, Operation, era ) where
import Control.Monad.Reader
import Control.Dangerous
import Control.DateTime.Offset hiding ( era )

data Directory

type Operation = DangerousT (Reader Directory)

era :: String -> Reader Directory [Offset]
