module TensorIO
    ( readTensor,
        parseStringsToInts,
        printVector
    ) where

parseStringToInts :: String -> [Double]
parseStringsToInts =
    fmap (read :: String -> Double) . words $

readVector :: IO [Double]
readVector =
    getLine >>= \line ->
    return $ parseStringToInts line
printVector :: [Double] -> IO()
printVector = putStrLn $ show