module Note
    ( Note(..)
    , Basic(modifiers)
    , parseBasic
    , parseNote
    , prefixes
    , suffixes
    ) where
import Control.Applicative
import Control.DateTime.Absolute
import Control.Modifier ( Modifier )
import Control.Name
import Data.Body ( Body, event )
import Data.Function ( on )
import Data.List ( sort )
import Data.Utils
import Data.Set ( Set )
import Internal
import Text.Fragment
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.TagWiki
import Text.Pin ( Pin )
import Text.Point ( Point(..), Side(..) )
import Text.Printf
import Text.Render
import qualified Control.Modifier as Mods
import qualified Data.Set as Set
import qualified Text.Pin as Pin
import qualified Text.Point as Point
import qualified Text.Symbols as Y

data Basic = Basic
    { _uid        :: Int
    , _names      :: [Name]
    , modifiers   :: [Modifier Pin]
    , _body       :: Body
    } deriving Show


-- | Helpers for people manipulating bodies in common cases
-- | (useful for most notes, but not notes in general)

-- The prefixes that were given (in order)
prefixes :: (Note a) => a -> [String]
prefixes = Mods.prefixes . modifiers . basic

-- The suffixes that were given (in order)
suffixes :: (Note a) => a -> [String]
suffixes = Mods.suffixes . modifiers . basic


instance Eq Basic where (==) = (==) `on` _uid
instance Ord Basic where (<=) = (<=) `on` _uid


class Note a where
    -- How to get the basic data
    basic :: a -> Basic

    -- A unique id; nice if it contains no spaces etc.
    -- the Int in `construct` will be unique, but sometimes you just want
    -- a prettier identifier.
    uid :: a -> Int
    uid = _uid . basic

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
    qualifiers :: a -> Set Pin
    qualifiers = Set.fromList . Mods.qualifiers . modifiers . basic

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

    -- Whether or not this file recognizes the categories and
    -- qualifiers on a pin (assumes that it has already respoded
    -- to the Pin.name)
    recognizes :: Pin -> a -> Bool
    recognizes p r = Pin.categories p `Set.isSubsetOf` categories r
                  && Pin.qualifiers p `Set.isSubsetOf` qualifiers r

    -- The primary name
    primaryName :: a -> String
    primaryName = headOr "" . map namePart . names

    -- Resolution of a point
    pointer :: (Internal i) => Maybe Point -> a -> i (Maybe Absolute)
    pointer Nothing r = pointer (Just Point{name="", side=Auto}) r
    pointer (Just pt) r = case event (Point.tag pt) (body r) of
        Just ev -> Just <$> when (side pt) ev
        Nothing -> pure Nothing

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

parseNote :: Int -> GenParser Char st Basic
parseNote i = parseBasic i Mods.catOrQual

parseBasic :: Int -> GenParser Char st (Modifier Pin) -> GenParser Char st Basic
parseBasic i modParser = do
    ns <- (parser `sepBy` designator Y.comma) <* eol
    ms <- modParser `manyTill` (whitespace *> eol)
    b <- parser
    pure $ Basic i ns ms b
