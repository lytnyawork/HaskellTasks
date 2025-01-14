collatz :: Integer -> Int
collatz n
    | n <= 0    = error "Число должно быть положительным"
    | n == 1    = 1
    | even n    = 1 + collatz (n `div` 2)
    | otherwise = 1 + collatz (3 * n + 1)

main :: IO ()
main = do
    putStrLn "Введите положительное число:"
    input <- getLine
    let number = read input :: Integer
    print $ "Длина сиракузской последовательности для числа " ++ show number ++ " равна " ++ show (collatz number)