fibonacci :: [Integer]
fibonacci = fibs 0 1
  where
    fibs a b = a : fibs b (a + b)


generalizedFibonacci :: [Integer] -> [Integer]
generalizedFibonacci initial = initial ++ genFibs (length initial)
  where
    genFibs m = let
        f n = sum $ take m (drop n fibs)
        fibs = 0 : 0 : genFibs m
      in map f [0..]

main :: IO ()
main = do
    print $ take 10 fibonacci  