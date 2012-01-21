module Body.Appearance ( Appearance(..) ) where
import Body.Unit
import Control.Monad
import Parsing
import Reference
import Text.Printf
import qualified Symbols as Y

data Appearance = Appearance { appRef  :: Reference
                             , appText :: [Unit]
                             } deriving Eq

instance Show Appearance where
    show (Appearance k xs) = printf "@%s %s" (show k)
        (if null xs then "" else "...")

instance Parseable Appearance where
    parser = (marker Y.appearance) >> liftM2 Appearance parser block
