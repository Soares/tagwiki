{-# LANGUAGE FlexibleInstances #-}
module Control.Appearance ( Appearance(..) ) where
import {-# SOURCE #-} Data.Directory ( location )
import Control.Applicative ( (<$>), (<*>) )
import Control.Unit ( Unit, block )
import Text.Fragment
import Text.ParserCombinators.TagWiki
import Text.Pinpoint
import Text.Render
import qualified Text.Symbols as Y

data Appearance = App
    { ref  :: Pinpoint
    , text :: [Unit]
    } deriving Eq

instance Show Appearance where
    show (App r _) = '@':show r

instance Parseable Appearance where
    parser = marker Y.appearance >> App <$> parser <*> block

instance Fragment Appearance where
    resolve (App r t) = article <$> location r <*> resolve t
