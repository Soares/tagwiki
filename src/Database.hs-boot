module Database ( Database, era ) where
import Control.Monad.Reader
import Text.DateTime.Offset hiding ( era )

data Database

era :: String -> Reader Database [Offset]
