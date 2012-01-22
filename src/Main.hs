module Main where
import Data.Functor
import System.Directory
import System.FilePath
import System.Exit
import Data.Head
import Data.Body

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
    exitFailure
