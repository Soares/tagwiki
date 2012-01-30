{-# LANGUAGE FlexibleInstances #-}
module Main where
import Control.Applicative
import Control.Dangerous hiding ( Warning )
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State hiding ( State )
import Data.Character
import Data.Directory
import Data.List ( sort, intercalate )
import Data.File
import Data.Either
import Data.Era
import Data.Maybe
import Data.Note hiding ( makeTag, tags )
import Data.Place
import Data.Record ( Record, alter )
import Data.State ( clean )
import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import Text.ParserCombinators.Parsec ( parse, ParseError )
import Text.Printf
import System.Console.GetOpt


data Options = Options
    { optRoot      :: FilePath
    , optSourceDir :: FilePath
    , optBuildDir  :: Maybe FilePath
    , optTagFile   :: Maybe FilePath
    , optHelp      :: Bool
    } deriving Show


defaultOptions :: Options
defaultOptions = Options
    { optRoot      = "."
    , optSourceDir = "src"
    , optBuildDir  = Nothing
    , optTagFile   = Just "tags"
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
        (ReqArg (\arg opt -> opt{optTagFile = Just arg}) "FILE")
        "the tag file to write to (default `tags`)"

    , Option "x" ["notags"]
        (NoArg (\opt -> opt{optTagFile = Nothing}))
        "do not write a tag file"

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
                , optBuildDir  = build
                , optSourceDir = src
                , optTagFile   = tagFile
                } = opts

    wikifiles <- locate $ root </> src
    let dir = foldr alter (new wikifiles) wikifiles

    when (isJust tagFile) $ do
        let dest = root </> fromJust tagFile
        let tupleToLine = uncurry makeTag
        let contents = unlines . sort . map tupleToLine $ tags dir
        writeFile dest contents

    when (isJust build) $ do
        let dest = root </> fromJust build
        let writer = writeFile . (dest </>)
        pairs <- dangerously $ runMomentable clean files dir
        mapM_ (uncurry writer) pairs


makeTag :: FilePath -> String -> String
makeTag filename tag = printf "%s\t%s\t:/^/" tag filename


locate :: FilePath -> IO [File]
locate src = do
    let visible = not . (== '.') . head
    filenames <- filter visible <$> getDirectoryContents src
    let paths = map (src </>) filenames
    (errs, wikifiles) <- partitionEithers <$> mapM load paths
    unless (null errs) $ mapM_ print errs *> exitFailure
    pure $ map File wikifiles


load :: FilePath -> IO (Either ParseError File)
load path = parse parser path <$> readFile path where
    parser = case takeExtension path of
        ".era" -> File <$> makeEra path
        ".char" -> File <$> makeCharacter path
        ".place" -> File <$> makePlace path
        _ -> File <$> makeNote path


-- | Unwraping database operations that can result in moments
-- | (All the piping is necessary to prevent infinite loops
-- | in date resolution.)
runMomentable :: State ->
                 StateT State (ReaderT Directory Dangerous) a ->
                 Directory ->
                 Dangerous a
runMomentable st fn = fmap fst . runReaderT (runStateT fn st)


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
