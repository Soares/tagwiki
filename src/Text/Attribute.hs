{-# OPTIONS_GHC -XFlexibleInstances #-}
module Text.Attribute ( Attribute(..) ) where
import Control.Monad
import Data.String.Utils
import Text.Fragment
import Text.ParserCombinators.TagWiki
import Text.ParserCombinators.Parsec
import Text.Printf
import Text.Render
import Text.Unit ( Unit, block )
import qualified Text.Symbols as Y

-- A key: value pair with an optional attribute block below
data Attribute = Attr { key   :: String
                      , value :: [Unit]
                      , text  :: [Unit] }
                           deriving Eq

instance Show Attribute where
    show (Attr k v _) = printf "%s: %s" (show k) (show v)

instance Parseable Attribute where
    parser = at >> liftM3 Attr (except " \t\n") (many parser) block
        where at = marker Y.attribute

instance Fragment [Attribute] where
    resolve db xs = section "Attributes" $ concatMap (resolve db) xs

instance Fragment Attribute where
    resolve db (Attr k v t) = article name (resolve db t)
        where name = printf "%s:  %s" (strip k) (resolve db v)
