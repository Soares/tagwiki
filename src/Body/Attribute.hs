module Body.Attribute ( Attribute(..) ) where
import Body.Unit
import Control.Monad
import Parsing
import Text.ParserCombinators.Parsec
import Text.Printf
import qualified Symbols as Y

-- A key: value pair with an optional attribute block below
data Attribute = Attribute { attrRef   :: String
                           , attrValue :: [Unit]
                           , attrBlock :: [Unit] }
                           deriving Eq

instance Show Attribute where
    show (Attribute k v xs) = printf "%s: %s %s" (show k) (show v)
        (if null xs then "" else "...")

instance Parseable Attribute where
    parser = (marker Y.attribute) >> liftM3 Attribute key (many parser) block
        where key = except " \t\n"
