# Notes on Haskell to keep in mind
## IO
IO is totally first class.

IO is an opaque type that behaves like any of the other monads we have encountered, like:
    - Maybe = Just a | Nothing
    - Either = Left a | Right b
    - Structure the way we created it
We use combinators to chain/compose IO subroutines.

IO is a monad that can have various types, like IO String or the unit type IO().

We have some IO combinators we should consider:
```
-- chaining IO operations: passing the *result* of the left IO operation
-- as an argument to the function on the right.
-- Pronounced "bind".
(>>=) :: IO a -> (a -> IO b) -> IO b

-- sequence two IO operations, discarding the payload of the first.
(*>) :: IO a -> IO b -> IO b

-- "lift" a value into IO context, does not add any I/O effects.
pure :: a -> IO a

-- "map" (or apply a function) over the payload value of an IO operation.
fmap :: (a -> b) -> IO a -> IO b
```

Let's look at some examples:
### Bind
```
-- A function that echoes the entered line
getLine >>= (\line -> putStrLn Line)

--The various types of sub-expressions here are:
getLine :: IO String
putStrLn :: String -> IO ()
{-
    So thanks to the bind combinator of type IO a -> (a -> IO b) -> IO b.
    We get \line :: String and the whole expressions resolves with types
    a :: String, b :: ()
-}

-- We can append two inputs as well
getLine >>=                     -- :: IO String (then bind)
    (\honorific                 -- :: String
        -> getLine >>=          -- :: IO String (then bind)
            (\name              -- :: String
                -> putStrLn ("Hello " ++ honorific ++ " " ++ name))) -- :: IO ()
```
### Sequence
```
-- chain IO operations and throw away prior io result
putStrLn "hello" *> putStrLn "world"
```
### pure // return
```
-- pure is a way to put a value into an IO without actually doing IO!
-- pure lifts 
confirm :: IO Bool
confirm =
  putStrLn "Are you sure? (y/n)" *>
    getLine >>= \answer ->  -- :: IO String -> String
      case answer of
        "y" -> pure True    -- :: Bool -> IO Bool
        "n" -> pure False   -- :: Bool -> IO Bool
        _ ->                -- :: sequence IO () -> IO String -> String -> IO Bool
          putStrLn "Invalid response. use y or n" *>
            confirm
```
### fmap
```
-- Takes a function and maps it into IO. So a String -> String function can act on IO.
-- We can convert:
getLine >>= \line -> pure (line ++ "!")
-- into 
fmap (\line -> line ++ "!") getLine
-- We are providing a function that appends an exclamation mark to a String
-- We then provide a function getLine of type IO String
-- the result of fmap will then return an IO String via pure
```
