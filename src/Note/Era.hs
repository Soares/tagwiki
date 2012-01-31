module Note.Era where
import Control.Applicative hiding ( (<|>) )
import Control.DateTime.Moment
import Control.Event ( at )
import Data.Body ( event )
import Data.Maybe ( fromMaybe )
import Note ( Note(..), Basic, parseBasic, prefixes, suffixes )
import Text.ParserCombinators.Parsec ( GenParser )
import Text.Point ( Side(Auto) )
import qualified Control.Modifier as Mods

newtype Era = Era { base :: Basic } deriving (Eq, Ord, Show)
instance Note Era where basic = base


codes :: Era -> [String]
codes = prefixes

precodes :: Era -> [String]
precodes = suffixes


instance Momentus Era where
    moment e = case event "dawn" $ body e of
        Just ev -> fromMaybe present <$> at Auto ev
        Nothing -> pure present


parseEra :: Int -> GenParser Char st Era
parseEra i = Era <$> parseBasic i Mods.anyMod
