isPerfect :: Integer -> Bool
isPerfect n
    | n <= 0    = False
    | otherwise = n == sum (divisors n)
  where
    divisors x = [d | d <- [1..(x `div` 2)], x `mod` d == 0]

main :: IO ()
main = do
    putStrLn "Введите число:"
    input <- getLine
    let number = read input :: Integer
    if isPerfect number
        then putStrLn $ show number ++ " является совершенным числом."
        else putStrLn $ show number ++ " не является совершенным числом."