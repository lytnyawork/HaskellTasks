gcd' :: Integral a => a -> a -> a
gcd' a 0 = a
gcd' a b = gcd' b (a `mod` b)

main :: IO ()
main = do
    putStrLn "Введите первое число:"
    aInput <- getLine
    putStrLn "Введите второе число:"
    bInput <- getLine
    
    let a = read aInput :: Int
        b = read bInput :: Int
        
    putStrLn $ "Наибольший общий делитель " ++ show a ++ " и " ++ show b ++ " равен " ++ show (gcd' a b)