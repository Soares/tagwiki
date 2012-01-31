{-# LANGUAGE ExistentialQuantification #-}
module Data.File where
import Data.Function
import Note

data File = forall a. Note a => File a
instance Show File where show (File f) = primaryName f
instance Eq File where (==) = (==) `on` uid
instance Ord File where (<=) = (<=) `on` uid
instance Note File where
    basic (File r) = basic r
    uid (File r) = uid r
    names  (File r) = names r
    categories (File r) = categories r
    qualifiers (File r) = qualifiers r
    body (File r) = body r
    tags (File r) = tags r
    primaryName (File r) = primaryName r
    pointer x (File r) = pointer x r
    recognizes x (File r) = recognizes x r
    text (File r) = text r
