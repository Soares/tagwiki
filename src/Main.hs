{-# LANGUAGE FlexibleInstances #-}
-- TODO: add subblock attributes
--      Use them for timezone & size on places
--      Use them for legend on events & appearances
module Main where
import Control.Applicative
import Control.Dangerous hiding ( Warning )
import Control.Dangerous.Extensions()
import Control.Monad
import Control.Monad.Reader
import Data.File
import Data.List ( sort, intercalate )
import Data.Maybe
import Data.Wiki
import Internal hiding ( when )
import Location ( normalize, display )
import Note ( text, parseNote )
import Note.Character
import Note.Era
import Note.Place
import System.Directory
import System.Environment
import System.FilePath
import Text.ParserCombinators.Parsec ( parse )
import Text.Printf
import System.Console.GetOpt

data Options = Options
    { optRoot      :: FilePath
    , optSourceDir :: FilePath
    , optBuildDir  :: Maybe FilePath
    , optTagFile   :: Maybe FilePath
    , optPlaceTree :: Bool
    , optHelp      :: Bool
    } deriving Show


defaultOptions :: Options
defaultOptions = Options
    { optRoot      = "."
    , optSourceDir = "src"
    , optBuildDir  = Nothing
    , optTagFile   = Nothing
    , optPlaceTree = False
    , optHelp      = False
    }


-- | A list of functions, each transforming the options data structure
-- | in response to a command-line option.
options :: [OptDescr (Options -> Options)]
options =
    [ Option "s" ["src"]
        (ReqArg (\arg opt -> opt{optSourceDir = arg}) "DIR")
        "directory containing the wiki source files"

    , Option "b" ["build"]
        (OptArg (\arg opt -> opt{optBuildDir = case arg of
                                                    Nothing -> Just "build"
                                                    Just x -> Just x
                                }) "DIR")
        "directory in which to place compiled files"

    , Option "t" ["tags"]
        (OptArg (\arg opt -> opt{optTagFile = case arg of
                                                   Nothing -> Just "tags"
                                                   Just x -> Just x
                                }) "FILE")
        "the tag file to write to (default `tags`)"

    , Option "p" ["tree"]
        (NoArg (\opt -> opt{optPlaceTree = True}))
        "output a tree of the places in the wiki"

    , Option "h" ["help"]
        (NoArg (\opt -> opt{optHelp = True}))
        "display this help"
    ]


addArgs :: [String] -> Options -> Dangerous Options
addArgs [] opts = return opts
addArgs [x] opts = return opts{optRoot = x}
addArgs (x:xs) opts = mapM_ (warn . ExtraArg) xs *> addArgs [x] opts


configure :: String -> [String] -> Dangerous Options
configure progname argv = do
    let (actions, args, unrecognized, errors) = getOpt' Permute options argv
    unless (null unrecognized) $
        mapM_ (warn . Unrecognized) unrecognized
    unless (null errors) $
        throw (OptErrors errors)
    config <- addArgs args $ foldr ($) defaultOptions actions
    when (optHelp config) $
        stop (DisplayHelp progname $ usageInfo progname options)
    return config


main :: IO ()
main = run =<< dangerously =<< configure <$> getProgName <*> getArgs


run :: Options -> IO ()
run opts = do
    let Options { optRoot      = root
                , optBuildDir  = bld
                , optSourceDir = src
                , optTagFile   = tagFile
                , optPlaceTree = drawTree
                } = opts

    files <- locate $ root </> src
    wiki <- foldM alter new $ zip [0..] files

    when (isJust tagFile) $ do
        let dest = root </> fromJust tagFile
        let tupleToLine = uncurry (makeTag $ root </> src)
        let contents = unlines . sort . map tupleToLine $ tagList wiki
        writeFile dest contents

    flip runInternal wiki $ do
        when (isJust bld) $ do
            let dest = root </> fromJust bld
            let writer = doWrite . (dest </>)
            liftIO $ clearDir dest
            build writer *> liftIO (putStrLn "")
        when drawTree $ do
            dict <- normalize =<< asks places
            let printLn = liftIO . putStrLn
            mapM_ printLn $ lines $ display dict


clearDir :: FilePath -> IO ()
clearDir dest = do
    exists <- doesDirectoryExist dest
    createDirectoryIfMissing exists dest
    files <- filter (not . (== '.') . head) <$> getDirectoryContents dest
    mapM_ (removeFile . (dest </>)) files


doWrite :: (MonadIO i, Internal i) => FilePath -> File -> i ()
doWrite filename file = do
    liftIO $ putStr "."
    txt <- text file
    liftIO $ writeFile filename txt


makeTag :: FilePath -> FilePath -> String -> String
makeTag src filename tag = printf "%s\t%s\t:/^/" tag (src </> filename)


locate :: FilePath -> IO [(FilePath, String)]
locate src = do
    let visible = not . (== '.') . head
    filenames <- filter visible <$> getDirectoryContents src
    let paths = map (src </>) filenames
    contents <- mapM readFile paths
    pure $ zip filenames contents


alter :: Wiki -> (Int, (FilePath, String)) -> IO Wiki
alter w (i, (path, txt)) = case takeExtension path of
    ".era" -> load recordEra parseEra
    ".char" -> load recordCharacter parseCharacter
    ".place" -> load recordPlace parsePlace
    _ -> load record parseNote
    where load rec par = case parse (par i) path txt of
                            Left err -> warn err *> pure w
                            Right note -> pure $ rec w path note


-- | Handling unexpected arguments
data Stop = DisplayHelp String String
instance Show Stop where
    show (DisplayHelp n s) = printf "%s: %s" n s

data Failure = OptErrors [String] | DirectoryMissing String FilePath
instance Show Failure where
    show (OptErrors es) = "failed to parse options:\n" ++ intercalate "\t" es
    show (DirectoryMissing n fp) = printf "%s directory missing: %s" n fp

data Warning = ExtraArg String | Unrecognized String
instance Show Warning where
    show (ExtraArg s) = "ignoring extra argument: " ++ s
    show (Unrecognized s) = "ignoring unrecognized flag: " ++ s
