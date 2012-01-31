{-# LANGUAGE FlexibleInstances #-}
module Main where
import Control.Applicative
import Control.Dangerous hiding ( Warning )
import Control.Dangerous.Extensions()
import Control.Monad
import Control.Monad.Reader
import Data.List ( sort, intercalate )
import Data.Maybe
import Data.Wiki
import Note ( parseNote )
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
                , optBuildDir  = bld
                , optSourceDir = src
                , optTagFile   = tagFile
                } = opts

    files <- locate $ root </> src
    wiki <- foldM alter new $ zip [0..] files

    when (isJust tagFile) $ do
        let dest = root </> fromJust tagFile
        let tupleToLine = uncurry makeTag
        let contents = unlines . sort . map tupleToLine $ tagList wiki
        writeFile dest contents

    when (isJust bld) $ do
        let dest = root </> fromJust bld
        let writer f txt = liftIO $ writeFile (dest </> f) txt
        runInternal (build writer) wiki *> pure ()


makeTag :: FilePath -> String -> String
makeTag filename tag = printf "%s\t%s\t:/^/" tag filename


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
