evalPolynomial :: [Integer] -> Integer -> Integer
evalPolynomial coeffs x = sum [c * (x ^ i) | (i, c) <- zip [0..] coeffs]

main :: IO ()
main = do
    let coefficients = [2, 1, 5]  -- Коэффициенты многочлена
        variableValue = 3         -- Значение переменной
    print $ "Значение многочлена при x = " ++ show variableValue ++ " равно " ++ show (evalPolynomial coefficients variableValue)