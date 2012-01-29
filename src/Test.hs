module Test where
import Control.Applicative
import Control.Dangerous
import Control.DateTime.Calculation
import Control.DateTime.Expression
import Control.Monad.Reader
import Data.Directory
import Data.List
import Data.Maybe
import Data.Note
import Data.Utils
import Debug.Trace ( trace )
import Main
import Text.Fragment
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.TagWiki
import Text.Pin ( Pin(..) )
import Text.Pinpoint ( Pinpoint(..) )
import Text.Point ( Point(..), Side(..) )
import Text.Reference
import Text.Utils
import qualified Control.DateTime.Moment as M
import qualified Data.Directory as D
import qualified Data.Map as Map
import qualified Data.Record as Record
import qualified Text.Pin as Pin
import qualified Text.Pinpoint as Pinpoint
import qualified Text.Point as Point

calcParser :: GenParser Char () Calculation
calcParser = parser
exprParser :: GenParser Char () Expression
exprParser = parser

parseExpr str = case parse exprParser "testing" str of Right x -> x
parseDate str = case parse calcParser "testing" str of Right x -> x
calc = parseDate "{100GE}"
calc2 = parseDate "{100GE, 1000GE}"
endKE = parseDate "{1628AT, +3}"

run fn dir = case runDangerous $ runMomentable fn dir of
    (Right r, ws) -> trace (warnings ws) r where
        warnings [] = ""
        warnings w = "warnings:\n\t" ++ intercalate "\t\n" (map show w)

findfile :: String -> Directory -> File
findfile str = head . filter ish . listing where
    ish file = Record.identifier file `like` str

defEvent file = run (firstEvent Nothing (Record.note file))

geoffrey = findfile "Geoffrey"
kyte = findfile "Kyte"
kyteStart = Both (Pin "Kyte" [] []) (Point Start "")


kytePin = Pin "Kyte" [] []
kytePoint = Point End ""
kytePP = Both kytePin kytePoint
kwe = Both (Pin "Kaolina War" [] []) (Point End "")
uwp = One (Pin "Unification War" [] [])
uwd = parseDate "{Unification War - 20 /1/2}"
missingDate = Both (Pin "Missing--" [] []) (Point End "")

make :: IO Directory
make = createDir <$> files

raeRef = (fromPin (Pin "Rae" [] []))
-- cands :: (Momentable m) => String -> m [File]
cands str = map candsFor <$> asks D.maps where
    candsFor = fromMaybe [] . Map.lookup (fromPin (Pin str [] []))
    stripNull = filter (not . null)
