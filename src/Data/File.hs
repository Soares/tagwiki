{-# LANGUAGE ExistentialQuantification #-}
module Data.File where
import Control.Dangerous.Extensions()
import Data.Function
import Data.Record ( Record )
import qualified Data.Record as Record

data File = forall a. Record a => File a
instance Show File where show (File f) = Record.name f
instance Eq File where (==) = (==) `on` Record.identifier
instance Ord File where (<=) = (<=) `on` Record.identifier
instance Record File where
    note (File r) = Record.note r
    dawn (File r) = Record.dawn r
    parent (File r) = Record.parent r
    alter (File r) = Record.alter r
