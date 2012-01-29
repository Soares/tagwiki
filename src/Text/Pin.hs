{-# LANGUAGE FlexibleInstances #-}
module Text.Pin ( Pin(..), empty ) where
import Control.Applicative hiding ( many, (<|>), empty )
import Data.List hiding ( find )
import Data.Set ( fromList )
import Data.String.Utils ( strip )
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.TagWiki
import Text.Printf
import Text.Utils
import qualified Control.Modifier as Mods
import qualified Text.Tag as Tag
import {-# SOURCE #-} Text.Pinpoint


-- A reference to another file and/or event
data Pin = Pin { tag        :: String
               , categories :: [String]
               , qualifiers :: [Pinpoint]
               } deriving Ord

instance Eq Pin where
    x == y = tag x `like` tag y && cs && qs where
        cs = fromList (categories x) == fromList (categories y)
        qs = fromList (qualifiers x) == fromList (qualifiers y)

empty :: Pin
empty = Pin{tag="", categories=[], qualifiers=[]}

instance Parseable Pin where
    parser = do
        name <- strip <$> Tag.tag
        mods <- many $ Mods.parse [Mods.category, Mods.qualifier]
        pure Pin{ tag = name
                , categories = Mods.categories mods
                , qualifiers = Mods.qualifiers mods }

instance Show Pin where
    show (Pin t cs qs) = printf "%s%s%s" (strip t)
        (if null cs then "" else " #"++intercalate "#" cs)
        (if null qs then "" else " ("++intercalate ") (" (map show qs) ++ ")")
