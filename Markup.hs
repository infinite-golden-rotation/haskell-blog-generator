module Markup
  ( Document,
    Structure (..),
  )
where

import Numeric.Natural

type Document =
  [Structure]

data Structure
  = Heading Natural String
  | Paragraph String
  | UnorderedList [String]
  | OrderedList [String]
  | CodeBlock [String]
  deriving (Show)

-- Define a function that will parse a string into a Document
-- Use newlines to determine each different line in string.
parse :: String -> Document
parse = parseLines [] . lines

parseLines :: [String] -> [String] -> Document
parseLines currentParagraph txts =
  let para = Paragraph (unlines (reverse currentParagraph))
   in case txts of
        [] -> [para] -- no more lines, return
        currentLine : rest ->
          -- there are lines, currentLine is consed out
          if trim currentLine == "" -- empty line, paragraph break
            then
              para : parseLines [] rest -- put accum para in output
            else
              parseLines (currentLine : currentParagraph) rest -- parse with currline prepended to currpara

trim :: String -> String
trim = unwords . words
