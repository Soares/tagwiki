module Data.Character where
import Data.Directory ( Key(..) )
import Data.Head ( Head )
import qualified Data.Head as Head
import Text.FuzzyString

-- TODO: ident/categories/qualifiers are redundant
load :: Head -> Key
load h = let names = charNames h in Key
    { ident = show h
    , tags = names
    , categories = Head.categories h
    , qualifiers = Head.qualifiers h
    , matches = fromHead h
    , offset = const Nothing
    , within = Nothing
    }

charNames :: Head -> [String]
charNames = const []

fromHead :: Head -> [(FuzzyString, Bool)]
fromHead h = concatMap (uncurry opts) (Head.names h) where
    cs = Head.categories h
    qs = Head.qualifiers h
    opts p n = map (\t -> (key t cs qs, p)) (makeTags h n)

makeTags :: Head -> String -> [String]
makeTags _ _ = []
