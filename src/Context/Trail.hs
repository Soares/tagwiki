module Context.Trail
    ( Trail(..)
    , ascendEra
    , ascendPinpoint
    , currentEra
    , currentPinpoint
    , descendEra
    , descendPinpoint
    , verifyEras
    , verifyPins
    , verifyTrail
    , home
    ) where
import Control.Applicative
import Data.List
import Data.Utils
import Text.Pinpoint

data Trail = Trail
    { eras       :: [String]
    , pinpoints  :: [Pinpoint]
    } deriving (Eq, Show)

ascendEra :: Trail -> Trail
ascendEra t = t{ eras = tail $ eras t }

ascendPinpoint :: Trail -> Trail
ascendPinpoint t = t{ pinpoints = tail $ pinpoints t }

currentEra :: Trail -> Maybe String
currentEra = maybeHead . eras

currentPinpoint :: Trail -> Maybe Pinpoint
currentPinpoint = maybeHead . pinpoints

descendEra :: String -> Trail -> Trail
descendEra e t = t{ eras = e:eras t }

descendPinpoint :: Pinpoint -> Trail -> Trail
descendPinpoint p t = t{ pinpoints = p:pinpoints t }

verifyEras :: Trail -> Bool
verifyEras = verify . eras

verifyPins :: Trail -> Bool
verifyPins = verify . pinpoints

verifyTrail :: Trail -> Bool
verifyTrail = (&&) <$> verifyEras <*> verifyPins

verify :: (Eq a) => [a] -> Bool
verify = (==) <*> nub

home :: Trail
home = Trail [] []
