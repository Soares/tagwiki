{-# LANGUAGE ExistentialQuantification #-}
module Data.File where
import Data.Function
import Note

data File = forall a. Note a => File a
instance Show File where show (File f) = primaryName f
instance Eq File where (==) = (==) `on` uid
instance Ord File where (<=) = (<=) `on` uid
instance Note File where basic (File r) = basic r
