module Main where

import HaskellBlog qualified as HB
import OptParse
import System.Directory (doesFileExist)
import System.Exit (exitFailure)
import System.IO

main :: IO ()
main = do
  options <- parse
  case options of
    -- directory -> directory case
    ConvertDir input output ->
      HB.convertDirectory input output
    -- single source-sink case
    ConvertSingle input output -> do
      -- Handle Input
      (title, inputHandle) <-
        case input of
          Stdin -> pure ("", stdin)
          InputFile file -> (,) file <$> openFile file ReadMode
      -- Handle Output
      outputHandle <-
        case output of
          Stdout -> pure stdout
          OutputFile file -> do
            exists <- doesFileExist file
            -- Confirm
            shouldOpenFile <-
              if exists
                then confirm
                else pure True
            if shouldOpenFile
              then openFile file WriteMode
              else exitFailure

      HB.convertSingle title inputHandle outputHandle
      hClose inputHandle
      hClose outputHandle

confirm :: IO Bool
confirm =
  putStrLn "Output file already exists. Are you sure you want to continue and overwrite?"
    *> getLine
    >>= \answer ->
      case answer of
        "y" -> pure True
        "n" -> pure False
        _ ->
          putStrLn ("Invalid input: [ " <> answer <> " ]. Use y or n.")
            *> confirm
