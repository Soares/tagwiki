module Database ( Database, Operation, era ) where
import Control.Monad.Reader
import Control.Dangerous
import Text.DateTime.Offset hiding ( era )

data Database

type Operation = DangerousT (Reader Database)

era :: String -> Reader Database [Offset]
