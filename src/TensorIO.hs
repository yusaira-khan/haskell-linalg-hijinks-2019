module TensorIO
    ( readVector,
        parseStringsToInts,
        printVector
    ) where

parseStringsToInts :: String -> [Double]
parseStringsToInts s =
    fmap (read :: String -> Double) . words $ s

readVector :: IO [Double]
readVector =
    getLine >>= \line ->
    return $ parseStringsToInts line
printVector :: [Double] -> IO()
printVector a = putStrLn $ show a