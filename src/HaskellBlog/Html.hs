{-
  We move all our old Html code to internal in case we want to allow for modifcation later.
-}
module HaskellBlog.Html
  ( Html,
    Title,
    Structure,
    html_,
    p_,
    h_,
    render,
    ul_,
    ol_,
    li_,
    code_,
    ANSIColor,
    Color,
    Brightness,
    EightColor,
  )
where

import HaskellBlog.Html.Colors
import HaskellBlog.Html.Internal
