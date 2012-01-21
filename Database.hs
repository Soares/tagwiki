module Database where
import Data.Map (Map)
import qualified Data.Map as Map

data Key = Key { keyTags       :: [Name]
               , keyCategories :: [Category]
               , keyQualifiers :: [Qualifier]
               , keyPrefixes   :: [Prefix]
               , keySuffixes   :: [Suffix]
               , keyTrials     :: [Trail]
               }

class File where
    -- The list of names that are atomic to the file
    names       :: File -> [Name] -> [Name]

    -- The list of tags that can be matched by an editor
    -- (This is a subset of all references that can be matched)
    tags        :: File -> [Name] -> [Modifier] -> [String]

    -- The data that needs to be maintained about the file
    file        :: Record -> File

    -- How to get the data in a standardized format
    attributes  :: File -> [Attribute]
    appearances :: File -> [Appearances]
    events      :: File -> [Event]
    text        :: File -> [Unit]


data Note = Note [Attribute] [Event] [Appearance] [Unit]
instance File Note where
    

data Character = Character [Attribute] [Event] [Appearance] [Unit]
data Place = Place (Maybe Reference) [Attribute] [Event] [Appearance] [Unit]
 

matches :: Key -> Reference -> Bool



{-
-- Tag → [Name]
names :: GenParser Char st [String]
names = name `sepBy` whitespace
quoted, name :: GenParser Char st String
quoted = between quote quote $ except "\""
name = try quoted <|> except "\"\t "
-}
