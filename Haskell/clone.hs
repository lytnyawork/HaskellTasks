clone :: Int -> [a] -> [a]
clone n xs
    | n <= 0    = []
    | otherwise = concatMap (replicate n) xs

main :: IO ()
main = do
    print $ clone 3 [1, 2, 3]  
    print $ clone 1 [1, 2, 3]  
    print $ clone 0 [1, 2, 3]