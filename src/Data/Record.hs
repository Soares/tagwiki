{-# LANGUAGE FlexibleInstances #-}
module Data.Record where
import Control.Applicative
import Data.Body
import Data.Maybe
import Data.Utils
import Text.Fragment
import Text.Pin ( Pin )
import Text.Reference ( Reference )
import Text.Render
import Text.Utils
import qualified Text.Reference as Ref
import {-# SOURCE #-} Control.DateTime.Moment
import {-# SOURCE #-} Data.Directory
import {-# SOURCE #-} Data.Note ( Note )
import {-# SOURCE #-} qualified Data.Note as Note

class (Eq a, Ord a) => Record a where
    -- How to construct the record
    construct :: FilePath -> Int -> a
    
    -- All our names
    -- Comes with a priority level attached
    -- Different from "refs" in that these are pretty and non-normalized.
    -- Differetn from "tags" in that all names are present.
    names :: a -> [Name]

    -- The categories that we belong to
    categories :: a -> [String]

    -- The qualifiers we recognize
    qualifiers :: a -> [Pinpoint]

    -- The source file, needed so that we can set jump locations
    -- for vim tags
    source :: a -> FilePath

    -- How to access the body of the record
    body :: a -> Body

    -- All the editor tags that we respond to
    -- For  many records this will be some small subset of `names`.
    tags :: a -> [String]

    -- How to get a pin from the file
    pin :: a -> Pin

    -- Why aren't Pin and Reference unified?
    ref :: a -> Reference
    ref = Ref.fromPin . pin

    -- All references that this record responds to
    -- ref should be `elem` refs
    refs :: Bool -> a -> [Reference]
    refs p f = Ref.keys cs qs ns where
        cs = Note.categories $ note f
        qs = Note.qualifiers $ note f
        ns = map snd . filter ((== p) . fst) . Note.names $ note f

    -- The primary name
    primaryName :: a -> String
    primaryName = headOr "" . map namePart . names

    -- Updates to directory maps etc.
    alter :: a -> Directory -> Directory
    alter = const id

    -- Render the record as text
    text :: (Momentable m) => a -> m String
    text r = ((top ++ "\n") ++) <$> bottom where
        top = concat [tit, hed, aka, cats, qals, toc]
        tit = title $ primaryName r
        hed = header $ primaryName r
        aka = section "Pseudonyms" (list $ drop 1 $ names r)
        cats = section "Categories" (list $ categories r)
        qals = section "Qualifiers" (list $ map show $ qualifiers r)
        toc = reference
            [ ("attributes", "Attributes")
            , ("appearances", "Appearances")
            , ("events", "Events")
            , ("notes", "Notes") ]
        bottom = resolve $ body r

    -- A unique id; nice if it contains no spaces etc.
    uid :: a -> String
    uid = slugify . name
