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
    note :: a -> Note

    pin :: a -> Pin
    pin r = Note.pin (identifier r) (note r)

    alter :: a -> Directory -> Directory
    alter = const id

    keys :: Bool -> a -> [Reference]
    keys p f = Ref.keys cs qs ns where
        cs = Note.categories $ note f
        qs = Note.qualifiers $ note f
        ns = map snd . filter ((== p) . fst) . Note.names $ note f

    filename :: a -> FilePath
    filename = slugify . identifier

    contents :: a -> Body
    contents = Note.body . note

    tags :: a -> [String]
    tags = Note.tags . note

    names :: a -> [String]
    names = map snd . Note.names . note

    name :: a -> String
    name = fromMaybe "" . maybeHead . names

    identifier :: a -> String
    identifier = normalize . name

    dawn :: (Momentable m) => a -> m (Maybe Moment)
    dawn = const $ pure Nothing

    parent :: a -> Maybe Pin
    parent = const Nothing

    text :: (Momentable m) => a -> m String
    text r = ((top ++ "\n") ++) <$> bottom where
        top = concat [tit, hed, aka, cats, qals, toc]
        tit = title $ name r
        hed = header $ name r
        aka = section "Pseudonyms" (list . drop 1 $ names r)
        cats = section "Categories" (list . Note.categories $ note r)
        qals = section "Qualifiers" (list . Note.qualifiers $ note r)
        toc = reference
            [ ("attributes", "Attributes")
            , ("appearances", "Appearances")
            , ("events", "Events")
            , ("notes", "Notes") ]
        bottom = resolve $ contents r
