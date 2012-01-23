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
    let get = (,) <$> (parser :: GenParser Char st Head)
                  <*> (parser :: GenParser Char st Body)
    return $ parse get fp txt


handle :: [ParseError] -> IO ()
handle errs = mapM_ print errs >> exitFailure

process :: [(Head, Body)] -> IO ()
process hbs = do
    let dir = Directory (map (uncurry note) hbs)
    tups <- execute $ runReader (runDangerousT files) dir
    print "ok then"
    print $ head tups
    -- mapM_ print $ taglist dirdir
