-- trying out recursive replicate impl to practice
replicate_ :: Int -> a -> [a]
replicate_ num val
  | num <= 0 = []
  | otherwise = val : replicate_ (num - 1) val

-- trying out mutual recursion example to practice
even_ :: Int -> Bool
even_ n
  | n < 0 = odd_ (n + 1)
  | n == 0 = True
  | otherwise = odd_ (n - 1)

odd_ :: Int -> Bool
odd_ n
  | n < 0 = even_ (n + 1)
  | n == 0 = False
  | otherwise = even_ (n - 1)
