module Main where

import Convert qualified as C
import Data.Functor (($>))
import Html qualified as H
import Markup qualified as M
import System.Directory (doesFileExist)
import System.Environment (getArgs)

-- Function that does parsing and conversion to html doc given a title and string
newtype HtmlString = HtmlString String -- added here to ensure writes only occur after processing

process :: H.Title -> String -> HtmlString
process title text =
  HtmlString $ H.render $ C.convert title $ M.parse text

-- Just a little function to pop String out of ProcessedContent
decon :: HtmlString -> String
decon (HtmlString c) = c

{-
    Argument patterns we allow:
      0 Args: Read stdin as input, write to stdout
      1 Arg:  (my own addition) read input file, write to stdout
      2 Args: first arg is input filename, second is output
        If the output filename already exists, request confirmation.
      otherwise:
-}

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> do
      stdin_ <- getContents
      putStrLn $ decon $ process "Untitled" stdin_
    [inputFile] -> do
      content <- readAlert inputFile
      putStrLn $ decon $ process inputFile content
    [inputFile, outputFile] -> do
      content <- readAlert inputFile
      writeAlert outputFile $ process inputFile content
    _ -> putStrLn "Usage: runghc BlogGenerator.hs [<input-file> [<output-file>]]"

-- version using regular combinators in do, we can also get rid of all the combinators altogether
{-
main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> getContents >>= \stdin_ -> putStrLn $ decon $ process "Untitled" stdin_
    [inputFile] -> readAlert inputFile >>= \content -> putStrLn $ decon $ process inputFile content
    [inputFile, outputFile] -> readAlert inputFile >>= \content -> writeAlert inputFile $ process inputFile content
    _ -> putStrLn "Usage: runghc Main.hs [-- <input-file> <output-file>]"
-}

-- added some warning on reads here
readAlert :: String -> IO String
readAlert filename = do
  exists <- doesFileExist filename
  let content
        | exists = readFile filename
        | otherwise = putStrLn (filename <> "does not exist!") $> ""
  content

-- put the write control flow into here, do not process in here on principle
writeAlert :: String -> HtmlString -> IO ()
writeAlert filename wrappedContent = do
  exists <- doesFileExist filename
  let writeResult
        | exists = conditionalAction (confirmOverwrite filename) (writeFile filename (decon wrappedContent))
        | otherwise = writeFile filename $ decon wrappedContent
  writeResult

confirmOverwrite :: String -> IO Bool
confirmOverwrite filename = do
  putStrLn ("Are you sure you want to overwrite " <> filename <> "? (y/n)")
  option <- getLine
  let choice
        | option == "y" = pure True
        | option == "n" = pure False
        | otherwise = putStrLn "Invalid response. Use y or n" *> confirm__ filename
  choice

conditionalAction :: (Monoid a) => IO Bool -> IO a -> IO a
conditionalAction cond act = do
  bcond <- cond
  let actResult
        | bcond = act
        | otherwise = mempty
  actResult

-- Old way using binds, we can use the syntactic sugar of do notation

process__ :: H.Title -> String -> String
process__ title text =
  H.render $ C.convert title $ M.parse text

main__ :: IO ()
main__ =
  getArgs >>= \args ->
    case args of
      [] -> getContents >>= \txt -> putStrLn $ process__ "Untitled" txt
      [input] -> readFile input >>= \txt -> putStrLn $ process__ input txt --
      [input, output] ->
        readFile input >>= \content ->
          doesFileExist output >>= \exists ->
            let writeResult = writeFile output (process__ input content)
             in if exists
                  then whenIO__ (confirm__ output) writeResult
                  else writeResult
      _ -> putStrLn "default usercase message"

confirm__ :: String -> IO Bool
confirm__ filename =
  putStrLn ("Are you sure you want to overwrite " <> filename <> "? (y/n)")
    *> getLine
    >>= \answer ->
      case answer of
        "y" -> pure True
        "n" -> pure False
        _ -> putStrLn "Invalid response. Use y or n" *> confirm__ filename

whenIO__ :: IO Bool -> IO () -> IO ()
whenIO__ cond action =
  cond >>= \result ->
    if result
      then action
      else pure ()
