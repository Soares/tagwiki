{-# LANGUAGE FlexibleInstances #-}
module Main where
import Control.Applicative
import Control.Dangerous
import Control.Monad.Reader
import Control.Monad.State
import Data.Directory
import Data.Either
import Data.Note
import Data.Record
import qualified Data.Map as Map
import System.Directory
import System.Exit
import System.FilePath
import Text.ParserCombinators.Parsec ( parse, ParseError )
import Text.ParserCombinators.TagWiki

src :: String
src = "/home/nate/Dropbox/Projects/LightAndAllHerColors/wiki/src"

main :: IO ()
main = do
    fs <- filter (not . (== '.') . head) <$> getDirectoryContents src
    (errs, notes) <- partitionEithers <$> mapM test fs
    if null errs then process (map File notes) else handle errs

test :: FilePath -> IO (Either ParseError Note)
test fp = parse parser fp <$> readFile (src </> fp)


handle :: [ParseError] -> IO ()
handle errs = mapM_ print errs >> exitFailure

process :: [File] -> IO ()
process notes = do
    let dir = Dir notes Map.empty Nothing
    execute $ runDangerous $ runReaderT checkForAmbiguities dir
    mapM_ (outputThrough dir) notes

-- type Operation = ReaderT Directory Dangerous
-- type RestrictedOperation = StateT [String] Operation

-- runMomentable :: (Momentable m) => Directory -> m a -> Dangerous a
runMomentable :: StateT [String] (ReaderT Directory Dangerous) a -> Directory -> Dangerous a
runMomentable wfn dir = fst <$> runReaderT (runStateT wfn []) dir

getText :: File -> Directory -> Dangerous String
getText file = runMomentable (text file)

outputThrough :: Directory -> File -> IO ()
outputThrough dir file = do
    let fn = filename file
    -- TODO: check for filename collisions
    txt <- execute $ runDangerous $ getText file dir
    putStrLn $ "\t\t" ++ fn
    putStrLn txt
