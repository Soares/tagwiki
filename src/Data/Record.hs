module Record where


contents :: (Key, Body) -> Operation String
contents (k, b) = ((top ++ "\n") ++) <$> bottom where
    -- TODO: these names are stupid
    top = concat [tit, hed, aka, cats, toc]
    nme = fromMaybe "" $ maybeHead $ tags k
    tit = title nme
    hed = header nme
    aka = section "Pseudonyms" (list $ drop 1 $ tags k)
    cats = section "Categories" (list $ categories k)
    toc = reference
        [ ("attributes", "Attributes")
        , ("appearances", "Appearances")
        , ("events", "Events")
        , ("notes", "Notes") ]
    bottom = resolve b

instance Record File where
    keys :: Bool -> File -> [String]
    filename :: File -> FilePath
    contents :: File -> Operation String
    tags     :: File -> [(FilePath, String)]
    (==)     :: File -> File -> Bool
    identifier :: File -> Operation String
    dawn     :: File -> Maybe Calculation
    parent :: File -> Maybe Pin
