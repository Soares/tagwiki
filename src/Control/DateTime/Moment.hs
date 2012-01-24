module Control.DateTime.Moment
    ( Moment(..)
    , Dateable(..)
    , present
    , plus
    , minus
    , invert
    , clobber
    ) where
import Control.Monad
import Control.Monad.Reader
import Data.Maybe
import {-# SOURCE #-} Data.Directory
import Control.DateTime.Era (Era)
import qualified Control.DateTime.Era as Era
import Control.DateTime.Utils
import Text.Printf

data Moment = Known [Maybe Int] (Maybe Era)
            | Unknown String
            deriving Eq

instance Show Moment where
    show k | k == present = "present"
    show (Unknown str) = printf "«Unknown: %s»" str
    show (Known ns er) = y ++ e ++ m ++ d ++ h ++ n ++ s ++ l where
        y = get 0
        e = maybe "" show er
        m = shw "/" 1
        d = shw "/" 2
        h = shw " " 3
        n = shw ":" 4
        s = shw "." 5
        l = shw "." 6
        shw str i = if null (get i) then "" else str ++ get i
        get i | i < length ns = maybe "" show (ns !! i)
              | otherwise = ""

class Dateable a where
    date :: a -> Operation Moment


present :: Moment
present = Known [] (Just Era.present)


-- Moment Operations
plus :: Moment -> Moment -> Operation Moment
plus (Unknown s) _ = return $ Unknown s
plus _ (Unknown s) = return $ Unknown s
plus (Known as ea) (Known bs eb) = asks relatable ea eb >>= plus' where
    plus' True = let er = first ea eb in do
        xs <- convert as er ea
        ys <- convert bs er eb
        return $ Known (add xs ys) er
    plus' False = return $ Unknown $ printf "Can't relate era %s to %s"
        (show ea) (show eb)

minus :: Moment -> Moment -> Operation Moment
minus a b = plus a (invert b)

invert :: Moment -> Moment
invert (Unknown s) = Unknown s
invert (Known _ (Just _)) = Unknown "Can't negate absolute dates"
invert (Known xs e) = Known (map (fmap (0-)) xs) e

clobber :: Moment -> Moment -> Moment
clobber _ (Unknown s) = Unknown s
clobber (Unknown s) _ = Unknown s
clobber (Known xs ex) (Known ys ey) = Known (zipAll second xs ys) (second ex ey)


-- Moment helpers

relatable :: Maybe Era -> Maybe Era -> Operation Bool
relatable Nothing _ = return True
relatable _ Nothing = return True
relatable (Just a) (Just b) = Era.relatable a b

convert :: [Maybe Int] -> Maybe Era -> Maybe Era -> Operation [Maybe Int]
convert xs Nothing _ = return xs
convert xs _ Nothing = return xs
convert xs (Just to) (Just from) = do
    diffs <- asks Era.difference to from
    return $ add xs (map Just diffs)

first :: Maybe a -> Maybe a -> Maybe a
first x y = if isNothing x then y else x

second :: Maybe a -> Maybe a -> Maybe a
second = flip first

add :: [Maybe Int] -> [Maybe Int] -> [Maybe Int]
add = zipAll $ liftM2 (+)
