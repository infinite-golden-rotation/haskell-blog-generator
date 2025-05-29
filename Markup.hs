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
              maybe id (:) context (parseLines Nothing rest)
            _ ->
              case context of
                Just (Paragraph para) ->
                  -- We did not get a newline line, so append line to context paragraph
                  parseLines (Just (Paragraph (unwords [para, line]))) rest
                _ ->
                  -- We had an empty context, so parse with new context
                  maybe id (:) context (parseLines (Just (Paragraph line)) rest)
