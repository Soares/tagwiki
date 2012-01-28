module Data.Utils where

headOr :: a -> [a] -> a
headOr x [] = x
headOr _ xs = head xs

maybeHead :: [a] -> Maybe a
maybeHead [] = Nothing
maybeHead (x:_) = Just x

firstJust :: [Maybe a] -> Maybe a
firstJust (Nothing:xs) = firstJust xs
firstJust (Just x:_) = Just x
firstJust [] = Nothing
