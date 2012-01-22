{-# OPTIONS_GHC -XFlexibleInstances #-}
module Text.Appearance ( Appearance(..) ) where
import Control.Monad
import Text.Fragment
import Text.ParserCombinators.TagWiki
import Text.Reference
import Text.Render
import Text.Unit ( Unit, block )
import qualified Text.Symbols as Y

data Appearance = App { ref  :: Reference
                      , text :: [Unit]
                      } deriving Eq

instance Show Appearance where
    show (App k _) = '@':show k

instance Parseable Appearance where
    parser = marker Y.appearance >> liftM2 App parser block

instance Fragment [Appearance] where
    resolve db xs = section "Appearances" $ concatMap (resolve db) xs

instance Fragment Appearance where
    resolve db (App r t) = article (resolve db r) (resolve db t)
