fromDigits :: Int -> [Int] -> Int
fromDigits n digits = sum [d * (n ^ i) | (i, d) <- zip [0..] (reverse digits)]

toDigits :: Int -> Int -> [Int]
toDigits n 0 = [0]
toDigits n x = reverse (toDigitsHelper x)
  where
    toDigitsHelper 0 = []
    toDigitsHelper y = let (q, r) = divMod y n
                       in r : toDigitsHelper q

addDigitwise :: Int -> [Int] -> [Int] -> [Int]
addDigitwise n a b = reverse $ addHelper (reverse a) (reverse b) 0
  where
    addHelper [] [] carry
      | carry == 0 = []
      | otherwise  = [carry]
    addHelper (x:xs) [] carry = let sum = x + carry
                                 in (sum `mod` n) : addHelper xs [] (sum `div` n)
    addHelper [] (y:ys) carry = let sum = y + carry
                                 in (sum `mod` n) : addHelper [] ys (sum `div` n)
    addHelper (x:xs) (y:ys) carry = let sum = x + y + carry
                                     in (sum `mod` n) : addHelper xs ys (sum `div` n)

main :: IO ()
main = do
    print $ fromDigits 2 [1, 0, 1, 1, 0, 1] 
    print $ toDigits 2 45                     
    print $ addDigitwise 2 [1, 0, 1, 1, 0, 1] [1, 1, 1] 