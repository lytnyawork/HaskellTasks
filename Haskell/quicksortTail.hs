quicksortTail :: Ord a => [a] -> [a]
quicksortTail xs = go xs []
  where
    go [] acc = acc
    go (p:xs) acc = go less (p : go greater acc)
      where
        less   = [x | x <- xs, x < p]
        greater = [x | x <- xs, x >= p]

mainTail :: IO ()
mainTail = do
    let list = [5, 3, 8, 1, 2, 7, 4, 6]
    print $ quicksortTail list