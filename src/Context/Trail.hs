module Context.Trail
    ( Trail(..)
    , ascendEra
    , ascendRef
    , currentEra
    , currentRef
    , descendEra
    , descendRef
    , verifyEras
    , verifyPins
    , verifyTrail
    , home
    ) where
import Context.Reference
import Control.Applicative
import Data.List
import Data.Utils

data Trail = Trail
    { eras  :: [String]
    , refs  :: [Reference]
    } deriving (Eq, Show)

ascendEra :: Trail -> Trail
ascendEra t = t{ eras = tail $ eras t }

ascendRef :: Trail -> Trail
ascendRef t = t{ refs = tail $ refs t }

currentEra :: Trail -> Maybe String
currentEra = maybeHead . eras

currentRef :: Trail -> Maybe Reference
currentRef = maybeHead . refs

descendEra :: String -> Trail -> Trail
descendEra e t = t{ eras = e:eras t }

descendRef :: Reference -> Trail -> Trail
descendRef r t = t{ refs = r:refs t }

verifyEras :: Trail -> Bool
verifyEras = verify . eras

verifyPins :: Trail -> Bool
verifyPins = verify . refs

verifyTrail :: Trail -> Bool
verifyTrail = (&&) <$> verifyEras <*> verifyPins

verify :: (Eq a) => [a] -> Bool
verify = (==) <*> nub

home :: Trail
home = Trail [] []
