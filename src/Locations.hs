module Locations where

data Bubble = Bubble File Size
tree :: Directory -> Tree Bubble
instance JSON Bubble
instance (JSON a) => JSON (Tree a)
