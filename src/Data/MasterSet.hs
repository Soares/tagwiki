module Data.MasterSet where
import Data.Set ( Set, isSubsetOf )
import qualified Data.Set as Set

data Role = Master | Slave deriving (Eq, Ord, Show)
data (Ord a) => MasterSet a = MasterSet
    { role :: Role
    , set  :: Set a
    } deriving Show

isMaster, isSlave :: (Ord a) => MasterSet a -> Bool
isMaster (MasterSet Master _) = True
isMaster _ = False
isSlave = not . isMaster

instance (Ord a) => Eq (MasterSet a) where
    x == y | role x == role y = set x == set y
           | isMaster x = set y `isSubsetOf` set x
           | isMaster y = set x `isSubsetOf` set y
           | otherwise = set x == set y

instance (Ord a) => Ord (MasterSet a) where
    compare x y | x == y = EQ
                | otherwise = compare (set x) (set y)


fromList :: (Ord a) => Role -> [a] -> MasterSet a
fromList r = MasterSet r . Set.fromList

toList :: (Ord a) => MasterSet a -> [a]
toList = Set.toList . set

union :: (Ord a) => MasterSet a -> MasterSet a -> MasterSet a
union (MasterSet x xs) (MasterSet _ ys) = MasterSet x (xs `Set.union` ys)

empty :: (Ord a) => Role -> MasterSet a
empty r = MasterSet r Set.empty
