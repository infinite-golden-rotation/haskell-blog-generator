module Markup
  ( Document,
    Structure (..),
    parse,
  )
where

import Data.Maybe (listToMaybe, maybeToList)
import Numeric.Natural

type Document =
  [Structure]

data Structure
  = Heading Natural String
  | Paragraph String
  | UnorderedList [String]
  | OrderedList [String]
  | CodeBlock [String]
  deriving (Eq, Show)

trim :: String -> String
trim = unwords . words

-- parse using Structures
parse :: String -> Document
parse = parseLines Nothing . lines -- New context usses Maybe Structure

parseLines :: Maybe Structure -> [String] -> Document
parseLines context txts =
  case txts of
    -- when there is no more text to parse, return the context accumulated
    [] -> maybeToList context
    -- Heading 1 case
    ('*' : ' ' : line) : rest ->
      maybe id (:) context (Heading 1 (trim line) : parseLines Nothing rest)
    -- UnorderedList case
    ('-' : ' ' : line) : rest ->
      case context of
        Just (UnorderedList list) ->
          -- case where we have list context, append line to UnorderedList
          parseLines (Just (UnorderedList (list <> [trim line]))) rest
        _ ->
          -- yield context and create new UnorderedList context
          maybe id (:) context (parseLines (Just (UnorderedList [trim line])) rest)
    -- OrderedList Case
    ('#' : ' ' : line) : rest ->
      case context of
        Just (OrderedList list) ->
          -- case where we have list context, append line to OrderedList
          parseLines (Just (OrderedList (list <> [trim line]))) rest
        _ ->
          -- yield context and create new OrderedList context
          maybe id (:) context (parseLines (Just (OrderedList [trim line])) rest)
    -- CodeBlock Case
    ('>' : ' ' : line) : rest ->
      case context of
        Just (CodeBlock list) ->
          -- case where we have list context, append line to OrderedList
          parseLines (Just (CodeBlock (list <> [trim line]))) rest
        _ ->
          -- yield context and create new OrderedList context
          maybe id (:) context (parseLines (Just (CodeBlock [trim line])) rest)
    -- Paragraph case
    currentLine : rest ->
      let line = trim currentLine
       in case line of
            "" ->
              -- Found empty line in input, separate paragraph, refresh context
              -- maybe takes three args (default, op, match_on)
              -- default is the default value, here id is returned if match_on is nothing
              -- op is the function we apply, here we are applying cons (:) to context and the rest of parseLines
              -- match_on is the value to consider, here that is the context which is of type Maybe Structure
              maybe id (:) context (parseLines Nothing rest)
            _ ->
              case context of
                Just (Paragraph para) ->
                  -- We did not get a newline line, so append line to context paragraph
                  parseLines (Just (Paragraph (unwords [para, line]))) rest
                _ ->
                  -- We had an empty context, so parse with new context
                  maybe id (:) context (parseLines (Just (Paragraph line)) rest)

{- Old impl, with new one we will use Structures to track context -}
-- Define a function that will parse a string into a Document
-- Use newlines to determine each different line in string.
parse_ :: String -> Document
parse_ = parseLines_ [] . lines

parseLines_ :: [String] -> [String] -> Document
parseLines_ currentParagraph txts =
  let para = Paragraph (unlines (reverse currentParagraph))
   in case txts of
        [] -> [para] -- no more lines, return
        currentLine : rest ->
          -- there are lines, currentLine is consed out
          if trim currentLine == "" -- empty line, paragraph break
            then
              para : parseLines_ [] rest -- put accum para in output
            else
              parseLines_ (currentLine : currentParagraph) rest -- parse with currline prepended to currpara

{- end old implementation -}

-- Using just normal pattern matching
isEmpty :: [a] -> Bool
isEmpty list =
  case list of
    [] -> True
    _ : _ -> False

-- Using listToMaybe
isEmpty_ :: [a] -> Bool
isEmpty_ list =
  case listToMaybe list of
    Nothing -> False
    Just _ -> True
