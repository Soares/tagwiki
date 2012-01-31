module Note where
import Control.Appearance ( Appearance )
import Control.Applicative
import Control.DateTime.Moment ( Moment )
import Control.Event ( at )
import Control.Modifier ( Modifier )
import Control.Name
import Data.Body ( Body(apps), event )
import Data.Function ( on )
import Data.List ( sort )
import Data.Utils
import Data.Set ( Set )
import Internal
import Text.Fragment
import Text.Pin ( Pin(Pin) )
import Text.Point ( Point(side), Side(..) )
import Text.Pinpoint ( Pinpoint )
import Text.Printf
import Text.Render
import Text.Utils
import qualified Control.Modifier as Mods
import qualified Data.Set as Set
-- TODO: remove Record and Record/Note.hs-boot

data Basic = Basic
    { _source     :: FilePath
    , _uid        :: Int
    , _names      :: [Name]
    , modifiers  :: [Modifier]
    , _body       :: Body
    }

instance Eq Basic where (==) = (==) `on` _uid
instance Ord Basic where (<=) = (<=) `on` _uid


class Note a where
    -- How to get the basic data
    basic :: a -> Basic

    -- A unique id; nice if it contains no spaces etc.
    -- the Int in `construct` will be unique, but sometimes you just want
    -- a prettier identifier.
    uid :: a -> String
    uid r = printf "%s-%d" (slugify $ primaryName r) (_uid $ basic r)

    -- All our names
    -- Comes with a priority level attached
    -- Different from "refs" in that these are pretty and non-normalized.
    -- Differetn from "tags" in that all names are present.
    names :: a -> [Name]
    names = _names . basic

    -- The categories that we belong to
    categories :: a -> Set String
    categories = Set.fromList . Mods.categories . modifiers . basic

    -- The qualifiers we recognize
    qualifiers :: a -> Set Pinpoint
    qualifiers = Set.fromList . Mods.qualifiers . modifiers . basic

    -- The source file, needed so that we can set jump locations
    -- for vim tags
    source :: a -> FilePath
    source = _source . basic

    -- How to access the body of the record
    body :: a -> Body
    body = _body . basic

    -- All the editor tags that we respond to
    -- For  many records this will be some small subset of `names`.
    tags :: a -> Set String
    tags x = Set.map (makeTag . namePart) nameset where
        makeTag n = unwords (n:map showq qlist)
        showq = printf "(%s)" . show
        qlist = sort  $ Set.toList $ qualifiers x
        nameset = Set.fromList $ names x

    -- How to get a pin from the file
    pin :: a -> Pin
    pin r = Pin (categories r) (qualifiers r) (primaryName r)

    -- All references that this record responds to
    -- ref should be `elem` refs
    pins :: Priority -> a -> [Pin]
    pins p r = map (Pin cs qs) ns where
        cs = categories r
        qs = qualifiers r
        ns = ofPriority p $ names r

    -- The primary name
    primaryName :: a -> String
    primaryName = headOr "" . map namePart . names

    -- Render the record as text
    text :: (Internal i) => a -> i String
    text r = ((top ++ "\n") ++) <$> bottom where
        top = concat [tit, hed, aka, cats, qals, toc]
        tit = title $ primaryName r
        hed = header $ primaryName r
        ns = map namePart $ names r
        cs = sort $ Set.toList $ categories r
        qs = sort $ Set.toList $ qualifiers r
        aka = section "Pseudonyms" (list $ drop 1 ns)
        cats = section "Categories" (list cs)
        qals = section "Qualifiers" (list $ map show qs)
        toc = reference
            [ ("attributes", "Attributes")
            , ("appearances", "Appearances")
            , ("events", "Events")
            , ("notes", "Notes") ]
        bottom = resolve $ body r


instance Note Basic where
    basic = id


-- TODO: uneccesary (?) function
firstEvent :: (Internal i) => Maybe Point -> Basic -> i (Maybe Moment)
firstEvent pt n = case event pt $ body n of
    Just ev -> at (maybe Start side pt) ev
    Nothing -> pure Nothing


firstAppearance :: Basic -> Maybe Appearance
firstAppearance = maybeHead . apps . body

{-
-- ====================================================
-- Everything below here is dead

-- | The names used internatlly to match pins
-- | Will be turned into Pins automatically
    , names      :: [(Bool, String)]
-- The tags to use for vim. There should be only one
-- (or one per pseudonym); use a fuzzy selector if you
-- want more flexibility.
    , tags       :: [String]

pin :: String -> Note -> Pin
pin s n = Pin s (categories n) (qualifiers n)

instance Record Note where
    note = id

instance Eq Note where
    x == y = ns && qs where
        ns = fromList (names x) == fromList (names y)
        qs = fromList (qualifiers x) == fromList (qualifiers y)

instance Ord Note where
    x <= y = pair x <= pair y where
        pair = fromList . names &&& fromList . qualifiers

makeNote :: FilePath -> GenParser Char st Note
makeNote fp = fst <$> parseNote fp Mods.catOrQual

firstEvent :: (Momentable m) => Maybe Point -> Note -> m (Maybe Moment)
firstEvent pt = Body.moment pt . body

firstAppearance :: Note -> Maybe Appearance
firstAppearance = maybeHead . Body.apps . body

parseNote :: FilePath -> GenParser Char st Modifier ->
                         GenParser Char st (Note, [Modifier])
parseNote fp parseMods = do
    ns <- firstLine
    mods <- parseMods `manyTill` (whitespace *> eol)
    let qs = Mods.qualifiers mods
    let cs = Mods.categories mods
    b <- parser
    pure (Note{ source = fp
              , names = ns
              , tags = map (makeTag qs . snd) ns
              , categories = cs
              , qualifiers = qs
              , body = b }, mods)

makeTag :: [Pinpoint] -> String -> String
makeTag qs n = unwords (n:map showq (sort qs)) where
    showq = printf "(%s)" . show

firstLine :: GenParser Char st [(Bool, String)]
firstLine = (name `sepBy` designator Y.comma) <* eol where
    name= whitespace >> (,) <$> priority <*> str where
        priority = option False (pri >> return True)
        escPri = hack >> pri
        str = (++) <$> option "" escPri <*> tag
        pri = string Y.priority



--- Warnings
data Warning = Ignored [String]
             | Unrecognized String
instance Show Warning where
    show (Ignored xs) = printf
        "Extra events ignored: [%s]" (intercalate ", " xs)
    show (Unrecognized x) = printf
        "Unrecognized event (resolved as !start) %s" x
        -}
