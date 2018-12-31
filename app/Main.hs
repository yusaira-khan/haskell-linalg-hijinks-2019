module Main where

import System.IO
import ListVector
import ListMatrix
import TensorIO

mat::ListMatrix.ListMatrix
mat = [[1,2],[3,4]]

vec::ListVector.ListVector
vec = [1,2,3,2,4,6]

-- main :: IO ()
-- main =
-- 	let av1 = readVector
-- 	in av1 >>= \a1 ->
--         let av2 = readVector
--         in av2 >>= \a2 ->
--         let mat = av1:av2:[]
--         in foldr (\v a -> printVector v) mat (putStr "")
main :: IO ()
main = TensorIO.printVector $ ListVector.listVectorSlice 1 0 3 vec