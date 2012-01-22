module Text.TagWiki.Appearance ( Appearance(..) ) where
import Control.Monad
import Text.Parser
import Text.Printf
import Text.TagWiki.Reference
import Text.TagWiki.Unit
import qualified Text.TagWiki.Symbols as Y

data Appearance = Appearance { appRef  :: Reference
                             , appText :: [Unit]
                             } deriving Eq

instance Show Appearance where
    show (Appearance k xs) = printf "@%s %s" (show k)
        (if null xs then "" else "...")

instance Parseable Appearance where
    parser = marker Y.appearance >> liftM2 Appearance parser block
