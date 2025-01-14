quicksort :: Ord a => [a] -> [a]
quicksort []     = []
quicksort (p:xs) = quicksort [x | x <- xs, x < p] ++ [p] ++ quicksort [x | x <- xs, x >= p]

main :: IO ()
main = do
    let list = [5, 3, 8, 1, 2, 7, 4, 6]
    print $ quicksort list