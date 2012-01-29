{-# LANGUAGE FlexibleInstances #-}
module Main where
-- TODO: imports
-- TODO: check language extensions
import Control.Applicative
import Control.Dangerous
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State hiding ( State )
import Data.Directory
import Data.Either
import Data.Note
import Data.Place
import Data.Era
import Data.Character
import Data.Record
import qualified Data.Map as Map
import System.Directory
import System.Exit
import System.FilePath
import Data.State ( clean )
import Text.ParserCombinators.Parsec ( parse, ParseError, GenParser )
import Text.ParserCombinators.TagWiki

src, dest :: String
src = "/home/nate/Dropbox/Projects/LightAndAllHerColors/wiki/src"
dest = "/home/nate/Dropbox/Projects/LightAndAllHerColors/wiki/build"

main :: IO ()
main = do
    ex <- doesDirectoryExist dest
    unless ex (createDirectoryIfMissing True dest)
    fs <- locate
    let dir = createDir fs
    pairs <- execute $ runDangerous $ runMomentable clean files dir
    mapM_ (uncurry $ writeFile . (dest </>)) pairs

createDir :: [File] -> Directory
createDir fs = foldr alter new fs where
    new = Dir fs Map.empty Map.empty

locate :: IO [File]
locate = do
    nms <- filter (not . (== '.') . head) <$> getDirectoryContents src
    (errs, fs) <- partitionEithers <$> mapM load nms
    unless (null errs) (mapM_ print errs >> exitFailure)
    return $ map File fs

load :: FilePath -> IO (Either ParseError File)
load f = parse parser' f <$> readFile (src </> f) where
    parser' = case takeExtension f of
        ".era" -> File <$> (parser :: GenParser Char st Era)
        ".char" -> File <$> (parser :: GenParser Char st Character)
        ".place" -> File <$> (parser :: GenParser Char st Place)
        _ -> File <$> (parser :: GenParser Char st Note)

runMomentable :: State ->
                 StateT State (ReaderT Directory Dangerous) a ->
                 Directory ->
                 Dangerous a
runMomentable st wfn dir = fst <$> runReaderT (runStateT wfn st) dir
