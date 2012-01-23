module Data.Note where
import Data.Directory ( Key(..) )
import Data.Head ( Head )
import qualified Data.Head as Head
import Text.FuzzyString

-- Todo: wtf?
note :: Head -> b -> (Key, b)
note h b = (load h, b)

load :: Head -> Key
load h = Key { ident = show h
             , name = case Head.names h of [] -> ""; ((_, n):_) -> n
             , pseudonyms = drop 1 $ map snd $ Head.names h
             , categories = Head.categories h
             , qualifiers = Head.qualifiers h
             , tags = map snd $ Head.names h
             , matches = fromHead h
             , offset = const Nothing
             , within = Nothing
             }

fromHead :: Head -> [(FuzzyString, Bool)]
fromHead h = map (\(b, n) -> (key n cs qs, b)) (Head.names h) where
    cs = Head.categories h
    qs = Head.qualifiers h
