module Data.Note where
import Control.Monad.Loops
import Control.Applicative
import Control.Reference ( Reference )
import qualified Control.Reference as Reference
import Data.Directory ( Key(..), Operation )
import Data.Head ( Head )
import Text.Utils
import qualified Data.Head as Head
import qualified Data.Set as Set

-- Todo: wtf?
note :: Head -> b -> (Key, b)
note h b = (load h, b)

load :: Head -> Key
load h = Key { ident = show h
             , name = case Head.names h of [] -> ""; ((_, n):_) -> n
             , pseudonyms = drop 1 $ map snd $ Head.names h
             , categories = Head.categories h
             , tags = map snd $ Head.names h
             , matches = matcher h
             , offset = const Nothing
             , within = Nothing
             }

matcher :: Head -> Reference -> Operation (Maybe Bool)
matcher h r = (\s -> andM [cats, quals, s]) <$> str where
    str = strMatch h (Reference.text r)
    cats = raise $ Head.categories h `hasAll` Reference.categories r
    quals = raise $ Head.qualifiers h `hasAll` Reference.qualifiers r
    raise x = if x then Just False else Nothing

hasAll :: [String] -> [String] -> Bool
x `hasAll` y = norm y `Set.isSubsetOf` norm x
    where norm = Set.fromList . map normalize 

strMatch :: Head -> String -> Operation (Maybe Bool)
strMatch h s = pure $ maximum $ map (match' s) (Head.names h) where
    match' x (p, n) = if x `like` n then Just p else Nothing
