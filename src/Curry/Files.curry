module Curry.Files where

import Directory       ( doesFileExist )
import FileGoodies     ( getFileInPath, lookupFileInPath )
import FilePath        ( takeFileName, (</>), (<.>) )
import ReadShowTerm    ( readUnqualifiedTerm ) -- for faster reading

import System.CurryPath    ( lookupModuleSourceInLoadPath, getLoadPathForModule
                           , inCurrySubdir, stripCurrySuffix )
import System.FrontendExec ( FrontendParams, FrontendTarget (..), defaultParams
                           , setQuiet, callFrontend, callFrontendWithParams
                           )

import Curry.Types

-- | Reads the short-AST from a specified module
readShortAST :: String -> IO (Module ())
readShortAST progname =
  readShortASTWithParseOptions progname (setQuiet True defaultParams)

-- | Reads the AST from a specified module
readFullAST :: String -> IO (Module ())
readFullAST progname =
  readFullASTWithParseOptions progname (setQuiet True defaultParams)

-- | Reads the short-AST with further options from a specified module
readShortASTWithParseOptions :: String -> FrontendParams -> IO (Module ())
readShortASTWithParseOptions progname options = do
  mbsrc <- lookupModuleSourceInLoadPath progname
  case mbsrc of
    Nothing -> do -- no source file, try to find shortAST file in load path
      loadpath <- getLoadPathForModule progname
      filename <- getFileInPath (shortASTFileName (takeFileName progname))
                                [""] loadpath
      readASTFile filename
    Just (dir,_) -> do
      callFrontendWithParams SAST options progname
      readASTFile (shortASTFileName (dir </> takeFileName progname))

-- | Reads the AST with further options from a specified module
readFullASTWithParseOptions :: String -> FrontendParams -> IO (Module ())
readFullASTWithParseOptions progname options = do
  mbsrc <- lookupModuleSourceInLoadPath progname
  case mbsrc of
    Nothing -> do -- no source file, try to find AST file in load path
      loadpath <- getLoadPathForModule progname
      filename <- getFileInPath (fullASTFileName (takeFileName progname))
                                [""] loadpath
      readASTFile filename
    Just (dir,_) -> do
      callFrontendWithParams AST options progname
      readASTFile (fullASTFileName (dir </> takeFileName progname))

-- | Get the short-AST filename of a Curry programm
shortASTFileName :: String -> String
shortASTFileName prog = inCurrySubdir (stripCurrySuffix prog) <.> "sast"

-- | Get the AST filename of a Curry programm
fullASTFileName :: String -> String
fullASTFileName prog = inCurrySubdir (stripCurrySuffix prog) <.> "ast"

-- | Reads the AST from a specified file
readASTFile :: String -> IO (Module ())
readASTFile filename = do
  filecontents <- readShortASTFileRaw filename
  -- read AST file...
  -- ...with generated Read class instances (slow!):
  --return (read filecontents)
  -- ...with built-in generic read operation (faster):
  return (readUnqualifiedTerm ["Curry.Types", "Curry.Ident", "Curry.Position",
                               "Curry.Span", "Curry.SpanInfo", "Prelude"]
                              filecontents)

-- | Reads the text from a specified file containing an AST
readShortASTFileRaw :: String -> IO String
readShortASTFileRaw filename = do
  extfcy <- doesFileExist filename
  if extfcy
   then readFile filename
   else do let subdirfilename = inCurrySubdir filename
           exdirtfcy <- doesFileExist subdirfilename
           if exdirtfcy
            then readFile subdirfilename
            else error ("EXISTENCE ERROR: AST file '" ++ filename ++
                        "' does not exist")
