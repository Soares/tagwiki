module Text.Fragment where

class Fragment a where
    resolve :: x -> a -> String

instance (Fragment a) => Fragment [a] where
    resolve db = concatMap (resolve db)
