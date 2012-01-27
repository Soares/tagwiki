{-# Language FlexibleInstances #-}
module Control.Unit ( Unit(..), section, block ) where
import Control.Applicative hiding ( (<|>), many )
import Control.Monad
import Control.DateTime.Calculation
import Text.Fragment
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.TagWiki
import Text.Pinpoint
import Text.Printf
import Text.Render ( link )
import qualified Text.Symbols as Y


data Unit = Str String
          | Dxp Calculation
          | Lnk Pinpoint [Unit]
          deriving Eq

instance Show Unit where
    show (Str s) = show s
    show (Dxp c) = show c
    show (Lnk p []) = printf "|%s|" (show p)
    show (Lnk p xs) = printf "|%s, %s|" (show p) (concatMap show xs)

instance Parseable Unit where
    parser = try (oLink >> Lnk <$> parser <*> (parser `manyTill` cLink))
         <|> try (Dxp <$> parser)
         <|> try (Str . pure <$> escWhite)
         <|> (Str <$> except Y.restrictedInText)
         <?> "simple unit (link|date|text)" where
        oLink = string Y.oLink >> pure ()
        cLink = string Y.cLink >> pure ()

instance Fragment Unit where
    resolve (Str s) = pure s
    resolve (Dxp c) = resolve c
    resolve (Lnk p []) = link <$> resolve p <*> pure (show p)
    resolve (Lnk p xs) = link <$> resolve p <*> resolve xs

instance Fragment [Unit] where
    resolve xs = concat <$> mapM resolve xs


-- Block that doesn't care about whitespace
section :: GenParser Char st [Unit]
section = try (pure . Str <$> many1 newline) <|> boringLine


-- The rest of the current line + all indented lines below
block :: GenParser Char st [Unit]
block = do
    clear <- liftM2 (++) (many parser) (pure . Str <$> eol)
    white <- lookAhead whitespace
    if null white then pure clear else do
        chunk <- many $ try $ blockWithWhite white
        pure $ clear ++ concat chunk


-- All indented lines below
blockWithWhite :: String -> GenParser Char st [Unit]
blockWithWhite w = pure . Str <$> many1 newline <|> lineWithWhite w


-- Lines with just the right amount of white
lineWithWhite :: String -> GenParser Char st [Unit]
lineWithWhite "" = boringLine
lineWithWhite (w:ws) = (char w >> lineWithWhite ws) <?> "moar whitespace!"


-- Lines that aren't attributes, events, or appearances
boringLine :: GenParser Char st [Unit]
boringLine = (do
    notFollowedBy (designator Y.event)
    notFollowedBy (designator Y.attribute)
    notFollowedBy (designator Y.appearance)
    many1 parser
    ) <?> "boring old line"
