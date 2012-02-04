{-# LANGUAGE ExistentialQuantification #-}
module Location where
import Control.Applicative
import Data.File()
import Data.Map ( Map )
import Data.Maybe ( mapMaybe )
import Data.Set ( Set, (\\) )
import Data.Tree ( Tree(Node), Forest )
import Note
import Internal
import Text.Pin
import qualified Data.Graph as Graph
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Tree as Tree

data (Note n) => Bubble n = Bubble
    { note :: n
    , size :: Double
    }

-- Internally resolve a map of i.e. Place -> Pin into a map of Place -> Place
-- TODO: where did Kaolina go?
-- TODO: where did Earth go?
normalize :: (Internal i, Ord n, Note n) => Map n Pin -> i (Map n n)
normalize orig = Map.foldWithKey rebuild (pure Map.empty) orig where
    rebuild k n idict = maybe idict update =<< resolve orig n where
        update v = Map.insert k v <$> idict

-- Find a note from a pin
resolve :: (Internal i, Ord n, Note n) => Map n Pin -> Pin -> i (Maybe n)
resolve dict pin = (findByUid . uid =<<) <$> find pin where
    findByUid i = List.find ((== i) . uid) $ Map.keys dict

-- Generate an edge, which contains the note, the uid of the note, and
-- TODO: what's happening to leaves?
-- Whene did Kindal go?
-- the uids of all children of the note
edge :: (Note n, Ord n) => Map n n -> n -> (n, Int, [Int])
edge dict n = (,,) n (uid n) (map uid $ children dict n)

-- Find all children of a note
children :: (Note n, Ord n) => Map n n -> n -> [n]
children dict n = Map.findWithDefault [] n $ invert dict

-- Flip a Child -> Parent mapping into a Parent -> [Children] mapping
invert :: (Note n, Ord n) => Map n n -> Map n [n]
invert = Map.foldWithKey inv Map.empty where
    inv k v = Map.insertWith (++) v [k]

-- Edges usable by Data.Graph
edges :: (Note n, Ord n) => Map n n -> [(n, Int, [Int])]
edges = map <$> edge <*> Map.elems

-- Find elements in a map that have no parents
roots :: (Note n, Ord n) => Map n n -> Set n
roots dict = Set.fromList (Map.elems dict) \\ Set.fromList (Map.keys dict)

-- Generate a tree from a map!
tree :: (Note n, Ord n) => Map n n -> Forest n
tree dict = map (fmap vertToNote) (Graph.dfs g vs) where
    (g, vertToEdge, uidToVert) = Graph.graphFromEdges $ edges dict
    vs = mapMaybe (uidToVert . uid) . Set.toList $ roots dict
    vertToNote = (\(n, _, _) -> n) . vertToEdge


display :: (Note n, Ord n, Show n) => Map n n -> String
display = Tree.drawTree . Node "(root)" . map (fmap show) . tree

{-
 - TODO: Tree -> Bubble Tree
    data Bubble = Bubble File Size
    tree :: Directory -> Tree Bubble
    instance JSON Bubble
    instance (JSON a) => JSON (Tree a)
-}
