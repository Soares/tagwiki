module Data.Trail
    ( Trail(..)
    , currentEra
    , currentFile
    , currentRef
    , descendEra
    , descendFile
    , descendRef
    , verify
    , home
    ) where
import {-# SOURCE #-} Text.Pinpoint
import {-# SOURCE #-} Data.Directory
import Data.Utils

data Trail = Trail { eraTrail  :: [String]
                   , refTrail  :: [Pinpoint]
                   , fileTrail :: [File]
                   } deriving (Eq, Show)

currentEra :: Trail -> Maybe String
currentEra = maybeHead . eraTrail

currentRef :: Trail -> Maybe Pinpoint
currentRef = maybeHead . refTrail

currentFile :: Trail -> Maybe File
currentFile = maybeHead . fileTrail

descendEra :: String -> Trail -> Trail
descendEra e t = t{eraTrail=e:eraTrail t}

descendRef :: Pinpoint -> Trail -> Trail
descendRef p t = t{refTrail=p:refTrail t}

descendFile :: File -> Trail -> Trail
descendFile p t = t{fileTrail=p:fileTrail t}

verify :: Trail -> Bool
verify t = verify' (refTrail t) && verify' (eraTrail t) where
    verify' (x:xs) = x `notElem` xs
    verify' [] = True

home :: Trail
home = Trail [] [] []
