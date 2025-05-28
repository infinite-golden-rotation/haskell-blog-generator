{-
  In hello_old we began the basics of creating an
  (E)mbedded (D)omain (S)pecific (L)anguage
  Here we will use types to ensure that all html we allow is well-formed.
  We want to make it sufficiently strict so only well-formed html is allowed in!
-}

{-
  There are two ways to implement types in haskell: type and newtype.

  When we do
    newtype Html = Html String
  we are defining a new datatype Html, which has a constructor Html :: String -> Html.

  The (newtype) Html on the left-side of the "=" lives in the *types* namespace thus
  it only exists on the *right-side* of a "::".
  The Html on the right-side is in the expressions/terms/values namespace, thus anywhere
  expressions are see, you will see it.

  Even though our Html is a string at the end of the day, this tells the compiler
  to tread it like its own thing. An "Html" structure.

  Now we restrict the compiler from allowing Html docs to be treated as Strings automatically.
  so "hello" <> Html "world" will fail to concat (and compile). This is good.

  The type keyword is used for clarity. We can use it like type Title = String and it allows us
  to create an alias of sorts for the String type. This type Title can be used interchangeably
  String.

-}
main = putStrLn (render myhtml)

myhtml = html_ "MyTitle" (append_ (h1_ "Heading") (append_ (p_ "Paragraph1") (p_ "Paragraph2")))
type Title = String

-- Let's create a newtype for our well-formed html document and another for structures therein
newtype Html = Html String
newtype Structure = Structure String

-- defn a function to render Html
render :: Html -> String
render (Html a) = a

{-
  We can pattern match in two ways:
  1. case expressions
      case <expression> of
        <pattern> -> <expression>
        ...
        <pattern> -> <expression>
    These are like switch statements where if an expr matches a pattern, we do some other expr.
  2. pattern matching on function arguments
      func <pattern> = <expression>
    This lets us use a function as a match statement.

  Wrapping and extracting expressions has no performance cost making this cool.
  This means we get type safety at no performance cost!
-}

-- a function to unpack our html structures from above
getStructureString :: Structure -> String
getStructureString struct = case struct of Structure str -> str

-- using the other pattern matching approach
getStructureString_ :: Structure -> String
getStructureString_ (Structure str) = str

-- Implement append for Structures
append_ :: Structure -> Structure -> Structure
append_ (Structure a) (Structure b) = Structure (a <> b) -- pattern match on args instead of manually deconstructing like below
-- append_ a b = Structure (getStructureString_ a <> getStructureString_ b)

-- we can compose functions using the (.) :: (b -> c) -> (a -> b) -> a -> c
-- operator which can compose functions f::(b->c) g::(a->b) to a function f.g::(a->c)
-- It takes three arguments: (.) f g x where x::a. If only two functions are provided we get the curried function described above.
-- Note also that lower case variables are called "type variables" which is just like a normal variable.

-- Curried composition of Structure::String->Structure and el::String->String
-- to get tag functions tag_::String->Structure
html_ :: Title -> Structure -> Html
html_ t (Structure s) = Html (el "html" (t <> s))

title_ :: String -> Structure
title_ = Structure . el "title"

body_ :: String -> Structure
body_ = Structure . el "body"

head_ :: String -> Structure
head_ = Structure . el "head"

p_ :: String -> Structure
p_ = Structure . el "p"

h1_ :: String -> Structure
h1_ = Structure . el "h1"

el :: String -> String -> String
el tag content = "<" <> tag <> ">" <> content <> "</" <> tag <> ">"
