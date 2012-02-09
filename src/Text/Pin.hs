{-# LANGUAGE FlexibleInstances #-}
module Text.Pin ( Pin(..), tag, empty, simple, isSelf, fromName ) where
import Control.Applicative hiding ( many, (<|>), empty, optional )
import Control.Name
import Data.Either
import Data.List hiding ( find )
import Data.Set ( Set, fromList )
import Data.String.Utils ( strip )
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.TagWiki
import Text.Printf
import Text.Utils
import qualified Control.Modifier as Mods
import qualified Data.Set as Set
import qualified Text.Tag as Tag


-- A reference to another file and/or event
data Pin = Pin
    { categories :: Set String
    , qualifiers :: Set Pin
    , text       :: String      -- The original text, for display
    }

tag :: Pin -> String            -- The normalized text, for checking equality
tag = slugify . text

empty :: Pin
empty = simple ""

simple :: String -> Pin
simple = Pin Set.empty Set.empty

isSelf :: Pin -> Bool
isSelf = (== "") . text

fromName :: Name -> Pin
fromName = simple . namePart

instance Eq Pin where
    x == y = tag x == tag y
             && categories x == categories y
             && qualifiers x == qualifiers y

instance Ord Pin where
    x <= y = tag x <= tag y
           && categories x <= categories y
           && qualifiers x <= qualifiers y

instance Parseable Pin where
    parser = do
        (names, mods) <- partitionEithers <$> many pinPart
        -- We need at least one name.
        -- If we failed to pick up a name in pinPart, pick one up now.
        -- This will probably cause an error, but at least it will be
        -- the 'right' "no name" error.
        name <- if null names then Tag.tag else pure $ unwords names
        pure Pin{ text = name
                , categories = fromList $ Mods.categories mods
                , qualifiers = fromList $ Mods.qualifiers mods }
        where pinPart = try (Left <$> Tag.tag)
                    <|> (Right <$> Mods.catOrQual)
                    <?> "text, category, or qualifier"

instance Show Pin where
    show (Pin cs qs t) = printf "%s%s%s" (strip t) cstr qstr where
        cstr = if null cs' then "" else " #" ++ intercalate " #" cs'
        qstr = if null qs' then "" else " (" ++ intercalate ") (" qs' ++ ")"
        cs' = Set.toList cs
        qs' = map show $ Set.toList qs
