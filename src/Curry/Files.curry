module Curry.Files where

import System.Directory (doesFileExist, getFileWithSuffix)
import System.FilePath  (takeFileName, (</>), (<.>))
import Distribution     ( FrontendParams, FrontendTarget (..), defaultParams
                        , setQuiet, inCurrySubdir, stripCurrySuffix
                        , callFrontend, callFrontendWithParams
                        , lookupModuleSourceInLoadPath, getLoadPathForModule
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
      filename <- getFileWithSuffix (shortASTFileName (takeFileName progname))
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
      filename <- getFileWithSuffix (fullASTFileName (takeFileName progname))
                                [""] loadpath
      readASTFile filename
    Just (dir,_) -> do
      callFrontendWithParams AST options progname
      readASTFile (shortASTFileName (dir </> takeFileName progname))

-- | Get the short-AST filename of a curry programm
shortASTFileName :: String -> String
shortASTFileName prog = inCurrySubdir (stripCurrySuffix prog) <.> "sast"

-- | Get the AST filename of a curry programm
fullASTFileName :: String -> String
fullASTFileName prog = inCurrySubdir (stripCurrySuffix prog) <.> "ast"

-- | Reads the AST from a specified file
readASTFile :: String -> IO (Module ())
readASTFile filename = do
  filecontents <- readShortASTFileRaw filename
  return (read filecontents)

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
