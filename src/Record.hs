module Record where

-- Unit data types
data FileType = Character | Place | Note deriving (Eq, Read, Show)
data Year = Year Int Era deriving (Eq, Read, Show)
type Category = String
type Qualifier = String
type Prefix = String
type Suffix = String
type Trail = String
type Era = String

-- A whole file
data Record = Record { recordFiletype    :: FileType
                     , recordTags        :: [Tag]
                     , recordModifiers   :: [Modifier]
                     , recordAttrs       :: [Attribute]
                     , recordEvents      :: [Event]
                     , recordAppearances :: [Appearance]
                     , recordUnits       :: [Unit]
                     } deriving (Eq, Read, Show)

-- Tags identifying the file
data Tag = Tag { tagPpriority :: Bool
               , tagName      :: String }
               deriving (Eq, Ord, Show, Read)

-- Modifiers on the tags
data Modifier = Cat Category
              | Qal Qualifier
              | Pre Prefix
              | Suf Suffix
              | Trl Trail
              deriving (Eq, Show, Read)

-- A chunk of text
data Unit = Str String
          | Lnk (Reference, Maybe String)
          | Dxp DateExpression
          deriving (Eq, Read, Show)

-- A key: value pair with an optional attribute block below
data Attribute = Attribute { attrKey   :: String
                           , attrValue :: [Unit]
                           , attrBlock :: [Unit] }
                           deriving (Eq, Read, Show)

-- A specification of an event
data Event = Event { eventName :: String
                   , eventWhen :: DateExpression
                   , eventText :: [Unit]
                   } deriving (Eq, Read, Show)

data Appearance = Appearance { appRef  :: String
                             , appText :: [Unit]
                             } deriving (Eq, Read, Show)

-- A reference to another file and/or event
data Reference = Reference { refTag        :: String
                           , refCategories :: [Category]
                           , refQualifiers :: [Qualifier]
                           , refEvents     :: [String]
                           } deriving (Eq, Read, Show)

-- A date that needs to be resolved
data DateExpression = Exactly Calc
                    | Range Calc Calc
                    deriving (Eq, Read, Show)

-- The means by which to resolve a date
data Calc = Plus Calc Calc
          | Minus Calc Calc
          | Clobber Calc Calc
          | From Reference
          | Simply When
          deriving (Eq, Show, Read)

data When = Abs AbsDate
          | Rel RelDate
          | At  Time
          deriving (Eq, Show, Read)

data AbsDate = AbsDate { absYear  :: Year
                       , absMonth :: Maybe Int
                       , absDay   :: Maybe Int
                       } deriving (Eq, Read, Show)

data RelDate = RelDate { relYear  :: Maybe Int
                       , relMonth :: Maybe Int
                       , relDay   :: Maybe Int
                       } deriving (Eq, Read, Show)

data Time = Time { hour   :: Maybe Int
                 , minute :: Maybe Int
                 , second :: Maybe Int
                 , detail :: Maybe Int
                 } deriving (Eq, Read, Show)


whenever = RelDate Nothing Nothing Nothing
noclock = Time Nothing Nothing Nothing Nothing
