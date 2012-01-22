module Main where
import Data.Functor
import System.Directory
import System.FilePath
import System.Exit
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.TagWiki
import Text.Record

src :: String
src = "/home/nate/Dropbox/Projects/LightAndAllHerColors/wiki/src"

main :: IO ()
main = do
    files <- filter (not . (== '.') . head) <$> getDirectoryContents src
    mapM_ load files
    print $ length files

load :: FilePath -> IO ()
load fp = do
    txt <- readFile (src </> fp)
    let result = parse (parser :: GenParser Char st Record) fp txt
    case result of
        Left err -> print err >> exitFailure
        Right _ -> putStr "."
