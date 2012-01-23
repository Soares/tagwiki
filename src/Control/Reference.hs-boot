module Control.Reference ( Reference(..) ) where
data Reference = Ref { text       :: String
                     , categories :: [String]
                     , qualifiers :: [String]
                     , events     :: [String]
                     }
