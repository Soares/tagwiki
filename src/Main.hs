module Main where
import Data.Either
import Data.Functor
import Control.Applicative
import Control.Dangerous
import Control.Monad.Reader
import System.Directory
import System.FilePath
import System.Exit
import Data.Note
import Data.Directory
import Text.ParserCombinators.Parsec ( parse, GenParser, ParseError )
import Text.ParserCombinators.TagWiki
import Data.Head
import Data.Body

src :: String
src = "/home/nate/Dropbox/Projects/LightAndAllHerColors/wiki/src"

main :: IO ()
main = do
    fs <- filter (not . (== '.') . head) <$> getDirectoryContents src
    (errs, hbs) <- partitionEithers <$> mapM test fs
    if null errs then process hbs else handle errs

test :: FilePath -> IO (Either ParseError (Head, Body))
test fp = do
    txt <- readFile (src </> fp)
    let prs = (,) <$> (parser :: GenParser Char st Head)
                  <*> (parser :: GenParser Char st Body)
    return $ parse prs fp txt


handle :: [ParseError] -> IO ()
handle errs = mapM_ print errs >> exitFailure

process :: [(Head, Body)] -> IO ()
process hbs = do
    let dir = Dir (map (uncurry note) hbs)
    fs <- execute $ runDangerous $ runReaderT files dir
    mapM_ (uncurry showf) fs

showf :: String -> String -> IO ()
showf k b = do
    putStrLn k
    putStrLn b
