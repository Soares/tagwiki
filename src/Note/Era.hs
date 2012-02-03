module Note.Era where
import Control.Applicative hiding ( (<|>) )
import Control.DateTime.Absolute
import Data.Body ( event )
import Data.Utils
import Internal
import Note ( Note(..), Basic, parseBasic, prefixes, suffixes )
import Text.ParserCombinators.Parsec ( GenParser )
import Text.Utils
import qualified Control.Modifier as Mods

newtype Era = Era { base :: Basic } deriving (Eq, Ord, Show)
instance Note Era where basic = base

code :: Era -> String
code e = headOr (show e) (prefixes e)

codes :: Era -> [String]
codes = map slugify . prefixes

precodes :: Era -> [String]
precodes = map slugify . suffixes


instance Momented Era where
    when side e = case event "dawn" $ body e of
        Just ev -> when side ev
        Nothing -> pure Present


parseEra :: Int -> GenParser Char st Era
parseEra i = Era <$> parseBasic i Mods.anyMod
