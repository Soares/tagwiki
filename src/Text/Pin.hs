{-# LANGUAGE FlexibleInstances #-}
module Text.Pin ( Pin(..), empty ) where
import Control.Applicative hiding ( many, (<|>), empty )
import qualified Control.Modifier as Mods
import Data.List hiding ( find )
import Data.String.Utils ( strip )
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.TagWiki
import Text.Printf
import qualified Text.Tag as Tag


-- A reference to another file and/or event
data Pin = Pin { tag        :: String
               , categories :: [String]
               , qualifiers :: [String]
               } deriving Eq

empty :: Pin
empty = Pin{tag="", categories=[], qualifiers=[]}

instance Parseable Pin where
    parser = do
        name <- Tag.tag
        mods <- many $ Mods.parse [Mods.category, Mods.qualifier]
        pure Pin{ tag = name
                , categories = Mods.categories mods
                , qualifiers = Mods.qualifiers mods }

instance Show Pin where
    show (Pin t cs qs) = printf "%s%s%s" (strip t)
        (if null cs then "" else " #"++intercalate "#" cs)
        (if null qs then "" else " ("++intercalate ") (" qs ++ ")")
