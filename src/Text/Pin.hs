{-# LANGUAGE FlexibleInstances #-}
module Text.Pin ( Pin(..), tag, empty, simple ) where
import Control.Applicative hiding ( many, (<|>), empty )
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
import {-# SOURCE #-} Text.Pinpoint


-- A reference to another file and/or event
data Pin = Pin
    { text       :: String      -- The original text, for display
    , categories :: Set String
    , qualifiers :: Set Pinpoint
    }

tag :: Pin -> String            -- The normalized text, for checking equality
tag = slugify . text

empty :: Pin
empty = Pin "" Set.empty Set.empty

simple :: String -> Pin
simple str = empty{text=str}

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
    show (Pin t cs qs) = printf "%s%s%s" (strip t) cstr qstr where
        cstr = if null cs' then "" else " #" ++ intercalate " #" cs'
        qstr = if null qs' then "" else " (" ++ intercalate ") (" qs' ++ ")"
        cs' = Set.toList cs
        qs' = map show $ Set.toList qs
