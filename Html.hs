{-
  We move all our old Html code to internal in case we want to allow for modifcation later.
-}
module Html
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

import Html.Colors
import Html.Internal
