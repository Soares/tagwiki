module Text.Fragment where
import Internal

class Fragment a where
    resolve :: (Internal i) => a -> i String
