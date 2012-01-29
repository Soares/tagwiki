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
import Data.Era
import Data.Record
import qualified Data.Map as Map
import System.Directory
import System.Exit
import System.FilePath
import Text.Printf
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
    fs <- filter (not . (== '.') . head) <$> getDirectoryContents src
    (errs, files) <- partitionEithers <$> mapM load fs
    if null errs then process (map File files) else handle errs

load :: FilePath -> IO (Either ParseError File)
load f = parse parser' f <$> readFile (src </> f) where
    parser' = case takeExtension f of
        "era" -> File <$> (parser :: GenParser Char st Era)
        _ -> File <$> (parser :: GenParser Char st Note)


handle :: [ParseError] -> IO ()
handle errs = mapM_ print errs >> exitFailure

process :: [File] -> IO ()
process files = do
    let dir = Dir files Map.empty Nothing
    execute $ runDangerous $ runReaderT checkForAmbiguities dir
    mapM_ (outputThrough dir) files

-- type Operation = ReaderT Directory Dangerous
-- type RestrictedOperation = StateT [String] Operation

-- runMomentable :: (Momentable m) => Directory -> m a -> Dangerous a
runMomentable :: StateT [String] (ReaderT Directory Dangerous) a -> Directory -> Dangerous a
runMomentable wfn dir = fst <$> runReaderT (runStateT wfn []) dir

getText :: File -> Directory -> Dangerous String
getText file = runMomentable (text file)

outputThrough :: Directory -> File -> IO ()
outputThrough dir file = do
    txt <- execute $ runDangerous $ getText file dir
    putStrLn $ printf "\tin %s\n" $ filename file
    writeFile (dest </> filename file) txt
