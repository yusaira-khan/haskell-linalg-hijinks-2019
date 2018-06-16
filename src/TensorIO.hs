module TensorIO
    ( readTensor,
    	parseStringsToInts,
    	printVector
    ) where

parseStringToInts :: String -> [Int]
parseStringsToInts =
    fmap (read :: String -> Int) . words $

readVector :: IO [Int]
readVector = 
	getLine >>= \line
	return $ parseStringToInts line
printVector :: [Int] -> IO()
printVector = putStrLn $ show 