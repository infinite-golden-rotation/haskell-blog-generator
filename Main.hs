module Main where

import Convert (convert)
import Html (Html, Title, html_, render)
import Markup (parse)

main :: IO ()
main = putStrLn "not implemented yet"

-- Function that does parsing and conversion to html doc given a title and string
process :: Title -> String -> String
process title text =
  render (convert title (parse text))
