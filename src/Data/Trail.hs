module Data.Trail
    ( Trail(..)
    , ascendEra
    , ascendFile
    , ascendRef
    , currentEra
    , currentFile
    , currentRef
    , descendEra
    , descendFile
    , descendRef
    , verify
    , home
    ) where
import Data.Utils
import Data.File
import Text.Pinpoint

data Trail = Trail
    { eraTrail  :: [String]
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

ascendEra :: Trail -> Trail
ascendEra t = t{eraTrail=tail $ eraTrail t}

ascendRef :: Trail -> Trail
ascendRef t = t{refTrail=tail $ refTrail t}

ascendFile :: Trail -> Trail
ascendFile t = t{fileTrail=tail $ fileTrail t}

verify :: Trail -> Bool
verify t = verify' (refTrail t) && verify' (eraTrail t) where
    verify' (x:xs) = x `notElem` xs
    verify' [] = True

home :: Trail
home = Trail [] [] []
