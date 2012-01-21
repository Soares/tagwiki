module Main where
import Data.Functor
import System.Directory
import System.FilePath
import System.Exit
import Text.ParserCombinators.Parsec
import Record
import Parser

src :: String
src = "/home/nate/Dropbox/Projects/LightAndAllHerColors/wiki/src"

main :: IO ()
main = do
    files <- filter (not . (== '.') . head) <$> getDirectoryContents src
    mapM_ check files
    print $ length files

check :: FilePath -> IO ()
check fp = do
    let mode = case takeExtensions fp of
                    ".char" -> Character
                    ".place" -> Place
                    _ -> Note
    text <- readFile (src </> fp)
    let result = parse (record mode) fp text
    case result of
        Left err -> print err >> exitFailure
        Right _ -> putStr "."
