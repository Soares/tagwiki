module Text.DateTime.Utils where
import Data.Maybe

intOrBlank :: Maybe Int -> String
intOrBlank = fromMaybe "" . fmap show

zipAll :: (a -> a -> a) -> [a] -> [a] -> [a]
zipAll _ [] a = a
zipAll _ a [] = a
zipAll f (x:xs) (y:ys) = f x y:zipAll f xs ys
