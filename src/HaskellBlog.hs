module HaskellBlog
  ( convertSingle,
    convertDirectory,
    process,
  )
where

import GHC.IO.Handle (Handle, hGetContents)
import GHC.IO.Handle.Text (hPutStrLn)
import HaskellBlog.Convert qualified as C
import HaskellBlog.Html qualified as H
import HaskellBlog.Markup qualified as M

process :: H.Title -> String -> String
process title text =
  H.render $ C.convert title $ M.parse text

convertSingle :: H.Title -> Handle -> Handle -> IO ()
convertSingle title input output = do
  content <- hGetContents input
  hPutStrLn output $ process title content

convertDirectory :: FilePath -> FilePath -> IO ()
convertDirectory = error "Directory conversion not implemented yet."
