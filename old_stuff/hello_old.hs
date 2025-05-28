{- Learning Haskell using https://learn-haskell.blog/02-hello.html -}

-- main procedure. Runs this first.
-- We can also assign the string to a name and then pass that in to putStrLn
main = putStrLn myhtml

myhtml :: String
myhtml = makeHtml "HELLO, WORLD" "I AM JAROOR!"

{- Function definitions look something like:
 -  function_name arg1 arg2 ... argN = expression
 -  Binary functions need to consider 'fixity'. The string concat operator has 'right-fixity' meaning:
 -  "hi" <> "I am" <> "jaroor" == "hi" <> ("I am" <> "jaroor")
 -  Functions also have precedence (a number between 1 and 10). Higher precedence means the operator goes first.
 -  + has precedence 6, * has 7, so:
 -  1+2*3 == 1+(2*3) == 7
 -  Basically, use parens to specify order of operations where unclear especially when using custom defined functions.
-}

-- A function using the ones defined below to create a header and such
-- Haskell uses indentation to group stuff. Avoid indentation styles that use more than one indentation step per layer.
makeHtml :: String -> (String -> String)
makeHtml header body_content = html_ (head_ (title_ header) <> "\n" <> body_ body_content)

-- Let's create functions to wrap strings in html and body tags
body_ :: String -> String
body_ = el "body"

html_ :: String -> String
html_ = el "html"

head_ :: String -> String
head_ = el "head"

title_ :: String -> String
title_ = el "title"

p_ :: String -> String
p_ = el "p"

-- we can use an anonymous function to write this as well, just here as an example.
-- anonfuncts can be used by defining args and outputs (\arg1 \arg2 -> arg1 + arg2)
h1_ :: String -> String
h1_ = (\htext -> el "h1" htext)

el :: String -> (String -> String)
el tag inner = "<" <> tag <> ">" <> inner <> "</" <> tag <> ">"

-- we can make anonymous functions anywhere like lambdas
