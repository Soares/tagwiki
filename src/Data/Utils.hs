module Data.Utils where

doHead :: b -> (a -> b) -> [a] -> b
doHead b _ [] = b
doHead _ fn (x:_) = fn x

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

zipAll :: (a -> a -> a) -> [a] -> [a] -> [a]
zipAll _ [] a = a
zipAll _ a [] = a
zipAll f (x:xs) (y:ys) = f x y:zipAll f xs ys

-- TODO: what _is_ this?
thread :: (Monad m) => (a -> m (Maybe a1)) -> Maybe a -> m (Maybe a1)
thread = maybe (return Nothing)
