{-# LANGUAGE FlexibleInstances #-}
module Data.Record where
import Control.Applicative
import Control.Modifier ( Modifier )
import Control.Name
import Data.Body
import Data.List ( sort )
import Data.Utils
import Data.Set ( Set )
import Text.Fragment
import Text.Pin ( Pin(Pin) )
import Text.Pinpoint ( Pinpoint )
import Text.Printf
import Text.Render
import Text.Utils
import qualified Control.Modifier as Mods
import qualified Data.Set as Set
import {-# SOURCE #-} Data.Directory

data Basic = Basic
    { _source     :: FilePath
    , _uid        :: Int
    , _names      :: [Name]
    , modifiers  :: [Modifier]
    , _body       :: Body
    }


class Note a where
    -- How to get the basic data
    basic :: a -> Basic

    -- How to construct the record
    construct :: FilePath -> Int -> a

    -- Updates to directory maps etc.
    alter :: a -> Directory -> Directory

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
    text :: (Momentable m) => a -> m String
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
    -- TODO: Typechecking placeholder
    construct s i = Basic s i [] [] (Body [] [] [] [])
    alter = const id
