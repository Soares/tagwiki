module Text.Reference ( Reference, key, keys, fromPin ) where
import Data.List ( sort )
import Data.Set ( Set )
import Text.Pin ( Pin(Pin) )
import Text.Utils ( normalize, like )
import qualified Data.Set as Set

data Reference = Ref { name       :: String
                     , categories :: Set String
                     , qualifiers :: Set String
                     , master     :: Bool
                     }

instance Show Reference where
    show (Ref n cs qs m) = show (Pin n c q) ++ t where
        t = if m then "[master]" else "[slave]"
        c = sort $ Set.toList cs
        q = sort $ Set.toList qs

instance Eq Reference where
    x == y | master y && not (master x) = y == x
    x == y | master x && not (master y) = ns && qs && cs where
        ns = name x `like` name y
        cs = categories x `hasAll` categories y
        qs = qualifiers x `hasAll` qualifiers y
    x == y = ns && qs && cs where
        ns = name x `like` name y
        cs = categories x `sameish` categories y
        qs = qualifiers x `sameish` qualifiers y

instance Ord Reference where
    m <= s | master m && not (master s) = (s == m) || (s > m)
    s <= m | master m && not (master s) = ns && qs && cs where
        ns = name s `lte` name m
        cs = categories m `hasAll` categories s
        qs = categories m `hasAll` categories s
    x <= y = ns && qs && cs where
        ns = name x `lte` name y
        cs = categories x `sameish` categories y
        qs = qualifiers x `sameish` qualifiers y


lte :: String -> String -> Bool
lte x y = normalize x <= normalize y

sameish :: Set String -> Set String -> Bool
sameish x y = (x `hasAll` y) && (y `hasAll` x)

hasAll :: Set String -> Set String -> Bool
hasAll x y = norm y `Set.isSubsetOf` norm x
    where norm = Set.map normalize

fromPin :: Pin -> Reference
fromPin (Pin n cs qs) = Ref n (Set.fromList cs) (Set.fromList qs) False

key :: [String] -> [String] -> String -> Reference
key cs qs n = Ref n (Set.fromList cs) (Set.fromList qs) True

keys :: [String] -> [String] -> [String] -> [Reference]
keys = (map .) . key
