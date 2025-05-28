module Html.Internal where

-- * Types

newtype Html
  = Html String

newtype Structure
  = Structure String
  deriving (Show)

instance Semigroup Structure where
  (<>) (Structure a) (Structure b) = Structure (a <> b)

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

ul_ :: [Structure] -> Structure
ul_ = Structure . el "ul" . list_

ol_ :: [Structure] -> Structure
ol_ = Structure . el "ol" . list_

list_ :: [Structure] -> String
list_ =
  let liElt s =
        (el "li" . escape) (getStructureString s)
   in concatMap liElt

li_ :: String -> String
li_ = el "li" . escape

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

-- safeHead to ensure we don't error out on empty String
safeHead :: [a] -> Maybe a
safeHead list =
  case list of
    [] -> Nothing
    x : _ -> Just x

exactlyTwo :: [a] -> Maybe (a, a)
exactlyTwo list =
  case list of
    [x, y] -> Just (x, y)
    _ -> Nothing
