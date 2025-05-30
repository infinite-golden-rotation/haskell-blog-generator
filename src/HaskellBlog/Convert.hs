module HaskellBlog.Convert where

import HaskellBlog.Html qualified as H
import HaskellBlog.Markup qualified as M

convert :: H.Title -> M.Document -> H.Html
convert title = H.html_ title . foldMap convertStructure

convertStructure :: M.Structure -> H.Structure
convertStructure struct =
  case struct of
    M.Heading lev txt -> H.h_ lev txt
    M.Paragraph txt -> H.p_ txt
    M.UnorderedList list -> H.ul_ $ map H.p_ list
    M.OrderedList list -> H.ol_ $ map H.p_ list
    M.CodeBlock list -> H.code_ $ unlines list
