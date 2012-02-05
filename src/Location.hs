module Location where
import Control.Applicative
import Data.List ( sortBy )
import Data.Map ( Map )
import Data.Maybe ( mapMaybe, isNothing )
import Data.Set ( Set )
import Data.Tree ( Tree(Node), Forest )
import Text.JSON
import qualified Data.Graph as Graph
import qualified Data.Map as Map
import qualified Data.Set as Set

-- Generate an edge, which contains the note, the uid of the note, and
edge :: (Ord n) => Map n (Maybe n) -> n -> (n, n, [n])
edge dict n = (,,) n n (children dict n)

-- Edges usable by Data.Graph
edges :: (Ord n) => Map n (Maybe n) -> [(n, n, [n])]
edges = map <$> edge <*> Map.keys

-- Generate a tree from a map!
-- Technically, we generate a forest, one tree from each root
forest :: (Ord n) => Map n (Maybe n) -> Forest n
forest dict = map (fmap vertToA) (Graph.dfs g vs) where
    (g, vertToEdge, idToVert) = Graph.graphFromEdges $ edges dict
    vs = mapMaybe idToVert . Set.toList $ roots dict
    vertToA = (\(n, _, _) -> n) . vertToEdge

-- Find all children of a note
children :: Ord o => Map o (Maybe o) -> o -> [o]
children dict n = Map.findWithDefault [] n $ invert dict

-- Flip a Child -> Parent mapping into a Parent -> [Children] mapping
invert :: Ord o => Map o (Maybe o) -> Map o [o]
invert = Map.foldWithKey inv Map.empty where
    inv k (Just v) = Map.insertWith (++) v [k]
    inv _ Nothing = id

-- Find elements in a map that have no parents
roots :: Ord o => Map o (Maybe o) -> Set o
roots = Set.fromList . Map.keys . Map.filter hasNoParent
    where hasNoParent = isNothing

sorted :: (Ord a) => Tree a -> Tree a
sorted (Node n ts) = Node n (sortBy sortNode $ map sorted ts)
    where sortNode (Node x _) (Node y _) = compare x y

unify :: a -> Forest a -> Tree a
unify _ [a] = a
unify a as = Node a as

class Bubble a where
    label  :: a -> String
    amount :: a -> Rational

bubbleTree :: (Bubble a) => Tree a -> JSValue
bubbleTree (Node a as) = JSObject $ toJSObject
    [ ("label", JSString $ toJSString $ label a)
    , ("amount", JSRational True $ amount a)
    , ("children", JSArray $ map bubbleTree as)
    ]
