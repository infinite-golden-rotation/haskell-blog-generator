-- This is where the main process will be.
-- Import the Html types and use them here.

import Html (Html, code_, h1_, html_, ol_, p_, render, ul_)

main :: IO ()
main = putStrLn (render myhtml)

myhtml :: Html
myhtml =
  html_
    "MyTitle"
    ( h1_ "Heading"
        <> p_ "Paragraph #1"
        <> p_ "Paragraph #2"
        <> ul_ [p_ "item 1", p_ "item 2", p_ "item 3"]
        <> ol_ [code_ "item 1", code_ "item 2", code_ "item 3"]
    )
