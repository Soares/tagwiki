{-# LANGUAGE FlexibleInstances #-}
module Main where
import Control.Applicative
import Control.Dangerous
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State hiding ( State )
import Data.Character
import Data.Directory
import Data.List ( sort )
import Data.File
import Data.Either
import Data.Era
import Data.Note hiding ( makeTag, tags )
import Data.Place
import Data.Record hiding ( tags )
import Data.State ( clean )
import System.Directory
import System.Exit
import System.FilePath
import Text.ParserCombinators.Parsec ( parse, ParseError )
import Text.Printf
import qualified Data.Map as Map

wiki, src, dest, tagFile :: String
wiki = "/home/nate/Dropbox/Projects/LightAndAllHerColors/wiki/"
src = wiki ++ "src"
dest = wiki ++ "build"
tagFile = wiki ++ "tags"

main :: IO ()
main = do
    ex <- doesDirectoryExist dest
    unless ex (createDirectoryIfMissing True dest)
    fs <- locate
    let dir = createDir fs
    let tagContents = unlines . sort . map (uncurry makeTag) $ tags dir
    writeFile tagFile tagContents
    pairs <- execute $ runDangerous $ runMomentable clean files dir
    mapM_ (uncurry $ writeFile . (dest </>)) pairs

makeTag :: FilePath -> String -> String
makeTag fn tag = printf "%s\t%s/%s\t:/^/" tag src fn

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
        ".era" -> File <$> makeEra f
        ".char" -> File <$> makeCharacter f
        ".place" -> File <$> makePlace f
        _ -> File <$> makeNote f

runMomentable :: State ->
                 StateT State (ReaderT Directory Dangerous) a ->
                 Directory ->
                 Dangerous a
runMomentable st wfn dir = fst <$> runReaderT (runStateT wfn st) dir
