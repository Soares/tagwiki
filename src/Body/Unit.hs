module Body.Unit ( Unit(..), section, block, restricted ) where
import Control.Monad
import Data.Functor
import Parsing
import DateTime
import Reference
import qualified Symbols as Y
import Text.ParserCombinators.Parsec
import Text.Printf


data Unit = Str String
          | Lnk (Reference, [Unit])
          | Dxp Calculation
          deriving Eq
instance Show Unit where
    show (Str s) = show s
    show (Lnk (ref, [])) = printf "|%s|" (show ref)
    show (Lnk (ref, xs)) = printf "|%s, %s|" (show ref) (concatMap show xs)
    show (Dxp c) = show c
instance Parseable Unit where
    parser = try (Lnk <$> link)
         <|> try (Dxp <$> parser)
         <|> (Str <$> text)
         <?> "simple unit (link|date|text)"


link :: GenParser Char st (Reference, [Unit])
link = oLink >> liftM2 (,) parser (parser `manyTill` cLink)


text :: GenParser Char st String
text = many1 $ try escWhite <|> escaping restricted


oLink, cLink :: GenParser Char st ()
oLink = string Y.oLink >> return ()
cLink = string Y.cLink >> return ()


restricted :: String
restricted = concat [Y.oLink, Y.oDate, "\n"]


-- Parsing unit chunks

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
        return $ clear ++ (concat chunk)

-- All the indented lines below
blockWithWhite :: String -> GenParser Char st [Unit]
blockWithWhite w = return . Str <$> many1 newline <|> lineWithWhite w

-- Lines with just the right amount of white
lineWithWhite :: String -> GenParser Char st [Unit]
lineWithWhite "" = boringLine
lineWithWhite (w:ws) = (char w >> lineWithWhite ws) <?> "moar whitespace!"

-- Lines that aren't keys, events, or appearances
boringLine :: GenParser Char st [Unit]
boringLine = (do
    notFollowedBy (designator Y.event)
    notFollowedBy (designator Y.attribute)
    notFollowedBy (designator Y.appearance)
    many1 parser
    ) <?> "boring old line"
