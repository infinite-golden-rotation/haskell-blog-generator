module HaskellBlog.Html.Internal where

import GHC.Natural (Natural)

-- * Types

newtype Html
  = Html String
  deriving (Show)

newtype Structure
  = Structure String
  deriving (Show)

instance Semigroup Structure where
  (<>) (Structure a) (Structure b) = Structure (a <> b)

-- Monoid like in algebra, a semigroup with an identity
-- here the mempty is an identity under mconcat, defined above
instance Monoid Structure where
  mempty = Structure ""

type Title =
  String

-- * EDSL

html_ :: Title -> Structure -> Html
html_ t (Structure s) =
  Html
    ( el
        "html"
        ( el "head" (el "title" (escape t))
            <> el "body" s
        )
    )

p_ :: String -> Structure
p_ = Structure . el "p" . escape

code_ :: String -> Structure
code_ = Structure . el "pre" . escape

h1_ :: String -> Structure
h1_ = Structure . el "h1" . escape

-- generalized header tagging function, header levels max out at 6 in html
h_ :: Natural -> String -> Structure
h_ lev
  | lev <= 6 = Structure . el ("h" <> show lev) . escape
  | otherwise = Structure . el "h6" . escape

ul_ :: [Structure] -> Structure
ul_ = Structure . el "ul" . concatMap (el "li" . getStructureString)

ol_ :: [Structure] -> Structure
ol_ = Structure . el "ol" . concatMap (el "li" . getStructureString)

li_ :: String -> String
li_ = el "li" . escape -- no esc

-- * Render

render :: Html -> String
render (Html h) = h

-- * Utilities

el :: String -> String -> String
el tag content = "<" <> tag <> ">" <> content <> "</" <> tag <> ">"

getStructureString :: Structure -> String
getStructureString (Structure s) = s

-- We need to escape some chars for html. let keyword binds a symbol locally, allows usage in in statment
escape :: String -> String
escape =
  let escapeChar c =
        case c of
          '<' -> "&lt;"
          '>' -> "&gt;"
          '&' -> "&amp;"
          '"' -> "&quot;"
          '\'' -> "&#39;"
          _ -> [c] -- String is the same as [char]
   in concat . map escapeChar
