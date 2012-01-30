{-# LANGUAGE FlexibleInstances #-}
module Text.Pin ( Pin(..), empty ) where
import Control.Applicative hiding ( many, (<|>), empty )
import Data.Either
import Data.List hiding ( find )
import Data.Set ( fromList )
import Data.String.Utils ( strip )
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.TagWiki
import Text.Printf
import Text.Utils
import Control.Modifier ( Modifier )
import qualified Control.Modifier as Mods
import qualified Text.Tag as Tag
import {-# SOURCE #-} Text.Pinpoint


-- A reference to another file and/or event
data Pin = Pin
    { tag        :: String
    , categories :: [String]
    , qualifiers :: [Pinpoint]
    } deriving Ord

instance Eq Pin where
    x == y = tag x `like` tag y && cs && qs where
        cs = fromList (categories x) == fromList (categories y)
        qs = fromList (qualifiers x) == fromList (qualifiers y)

empty :: Pin
empty = Pin{tag="", categories=[], qualifiers=[]}


pinPart :: GenParser Char st (Either String Modifier)
pinPart = try (Left . strip <$> Tag.tag)
      <|> (Right <$> Mods.catOrQual)
      <?> "text, category, or qualifeir"

instance Parseable Pin where
    parser = do
        (names, mods) <- partitionEithers <$> many pinPart
        name <- if null names then strip <$> Tag.tag
                              else pure $ unwords names
        pure Pin{ tag = name
                , categories = Mods.categories mods
                , qualifiers = Mods.qualifiers mods }

instance Show Pin where
    show (Pin t cs qs) = printf "%s%s%s" (strip t)
        (if null cs then "" else " #"++intercalate "#" cs)
        (if null qs then "" else " ("++intercalate ") (" (map show qs) ++ ")")
