module Head where

-- Tags identifying a file
data Name = Name { namePriority :: Bool
                 , nameText     :: String
                 } deriving (Eq, Ord, Show, Read)


-- Modifiers on the tags
data Modifier = Category String
              | Qualifier String
              | Prefix String
              | Suffix String
              | Trail String
              deriving (Eq, Show, Read)
categories, qualifiers, prefixes, suffixes, trails :: [Modifier] -> [String]
categories xs = [y | Category y <- xs]
qualifiers xs = [y | Qualifier y <- xs]
prefixes xs = [y | Prefix y <- xs]
suffixes xs = [y | Suffix y <- xs]
trails xs = [y | Trail y <- xs]
separate :: [Modifier] -> ([String], [String], [String], [String], [String])
separate xs = (categories xs, qualifiers xs, prefixes xs, suffixes xs, trails xs)
