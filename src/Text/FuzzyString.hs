module Text.FuzzyString ( FuzzyString, key, fromRef ) where
import Control.Reference ( Reference(Ref) )
import Data.Set ( Set )
import Data.List ( sort )
import qualified Data.Set as Set
import Text.Utils ( normalize )

data FuzzyString = Fuz { name       :: String
                       , categories :: Set String
                       , qualifiers :: Set String
                       , master     :: Bool
                       }

instance Show FuzzyString where
    show (Fuz n cs qs m) = show (Ref n c q [ms]) where
        ms = if m then "master" else "slave"
        c = sort $ Set.toList cs
        q = sort $ Set.toList qs

instance Eq FuzzyString where
    x == y | master y && not (master x) = y == x
    x == y | master x && not (master y) = ns && qs && cs where
        ns = name x `like` name y
        cs = categories x `hasAll` categories y
        qs = qualifiers x `hasAll` qualifiers y
    x == y = ns && qs && cs where
        ns = name x `like` name y
        cs = categories x `sameish` categories y
        qs = qualifiers x `sameish` qualifiers y

instance Ord FuzzyString where
    m <= s | master m && not (master s) = (s == m) || (s > m)
    s <= m | master m && not (master s) = ns && qs && cs where
        ns = name s `lte` name m
        cs = categories m `hasAll` categories s
        qs = categories m `hasAll` categories s
    x <= y = ns && qs && cs where
        ns = name x `lte` name y
        cs = categories x `sameish` categories y
        qs = qualifiers x `sameish` qualifiers y


like :: String -> String -> Bool
like x y = normalize x == normalize y

lte :: String -> String -> Bool
lte x y = normalize x <= normalize y

sameish :: Set String -> Set String -> Bool
sameish x y = (x `hasAll` y) && (y `hasAll` x)

hasAll :: Set String -> Set String -> Bool
hasAll x y = norm y `Set.isSubsetOf` norm x
    where norm = Set.map normalize 

fromRef :: Reference -> FuzzyString
fromRef (Ref n cs qs _) = Fuz n (Set.fromList cs) (Set.fromList qs) False

key :: String -> [String] -> [String] -> FuzzyString
key n cs qs = Fuz n (Set.fromList cs) (Set.fromList qs) True
