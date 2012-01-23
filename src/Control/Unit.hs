{-# Language FlexibleInstances #-}
module Control.Unit ( Unit(..), section, block ) where
import Control.Applicative ( (<$>), (<*>) )
import Control.Monad
import Control.DateTime.Calculation
import Text.Fragment
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.TagWiki
import Text.Printf
import Control.Reference
import Text.Render ( link )
import qualified Text.Symbols as Y


data Unit = Str String
          | Lnk Reference [Unit]
          | Dxp Calculation
          deriving Eq

instance Show Unit where
    show (Str s) = show s
    show (Lnk ref []) = printf "|%s|" (show ref)
    show (Lnk ref xs) = printf "|%s, %s|" (show ref) (concatMap show xs)
    show (Dxp c) = show c

instance Parseable Unit where
    parser = try (oLink >> Lnk <$> parser <*> (parser `manyTill` cLink))
         <|> try (Dxp <$> parser)
         <|> try (Str . return <$> escWhite)
         <|> (Str <$> except Y.restrictedInText)
         <?> "simple unit (link|date|text)"

instance Fragment Unit where
    resolve (Str s) = return s
    resolve (Lnk ref xs) = link <$> src <*> txt where
        txt = if null xs then resolve ref else resolve xs
        src = source ref
    resolve (Dxp c) = resolve c

instance Fragment [Unit] where
    resolve xs = concat <$> mapM resolve xs


oLink, cLink :: GenParser Char st ()
oLink = string Y.oLink >> return ()
cLink = string Y.cLink >> return ()


-- Block that doesn't care about whitespace
section :: GenParser Char st [Unit]
section = try (return . Str <$> many1 newline) <|> boringLine


-- The rest of the current line + all indented lines below
block :: GenParser Char st [Unit]
block = do
    clear <- liftM2 (++) (many parser) (return . Str <$> eol)
    white <- lookAhead whitespace
    if null white then return clear else do
        chunk <- many $ try $ blockWithWhite white
        return $ clear ++ concat chunk


-- All indented lines below
blockWithWhite :: String -> GenParser Char st [Unit]
blockWithWhite w = return . Str <$> many1 newline <|> lineWithWhite w


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
