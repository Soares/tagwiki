{-# LANGUAGE FlexibleInstances #-}
module Main where
import Control.Applicative
import Control.Dangerous
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import Data.Directory
import Data.Either
import Data.Note
import Data.List
-- TODO: Place
import Data.Era
import Data.Character
import Data.Record
import qualified Data.Map as Map
import System.Directory
import System.Exit
import System.FilePath
import Data.Trail ( Trail, home )
import Text.ParserCombinators.Parsec ( parse, ParseError, GenParser )
import Text.ParserCombinators.TagWiki

src, dest :: String
src = "/home/nate/Dropbox/Projects/LightAndAllHerColors/wiki/src"
dest = "/home/nate/Dropbox/Projects/LightAndAllHerColors/wiki/build"

mkDir :: FilePath -> IO ()
mkDir path = do
  ex <- doesDirectoryExist path
  unless ex (createDirectoryIfMissing True path)

main :: IO ()
main = do
    mkDir dest
    fs <- files
    let dir = createDir fs
    mapM_ print (allRefs dir)
    mapM_ (outputThrough dir) fs

createDir :: [File] -> Directory
createDir fs = foldr alter new fs where
    new = Dir fs Map.empty Nothing

files :: IO [File]
files = do
    nms <- filter (not . (== '.') . head) <$> getDirectoryContents src
    (errs, fs) <- partitionEithers <$> mapM load nms
    unless (null errs) (handle errs)
    return $ map File fs

load :: FilePath -> IO (Either ParseError File)
load f = parse parser' f <$> readFile (src </> f) where
    parser' = case takeExtension f of
        ".era" -> File <$> (parser :: GenParser Char st Era)
        ".char" -> File <$> (parser :: GenParser Char st Character)
        _ -> File <$> (parser :: GenParser Char st Note)


handle :: [ParseError] -> IO ()
handle errs = mapM_ print errs >> exitFailure

getText :: File -> Directory -> Dangerous String
getText file = runMomentable (text file) where

runMomentable :: StateT Trail (ReaderT Directory Dangerous) a
                    -> Directory -> Dangerous a
runMomentable wfn dir = fst <$> runReaderT (runStateT wfn home) dir

outputThrough :: Directory -> File -> IO ()
outputThrough dir file = do
    putStrLn $ "(in " ++ show file ++ ")"
    txt <- execute $ runDangerous $ getText file dir
    writeFile (dest </> filename file) txt

showFiles :: Directory -> IO ()
showFiles = mapM_ print . sort . allRefs
