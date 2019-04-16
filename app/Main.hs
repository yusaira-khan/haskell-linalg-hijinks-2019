module Main where

import System.IO
import ListVector
import ListMatrix
import TensorIO

mat2::ListMatrix.ListMatrix
-- 1 3
-- 2 4
mat2 = [[1,2],[3,4]]


mat3::ListMatrix.ListMatrix
-- 1 3 2
-- 2 1 3
-- 3 2 1
mat3 = [[1,2,3],[3,1,2],[2,3,1]]

vec::ListVector.ListVector
vec = [1,2,3,2,4,6]

mat23 :: ListMatrix.ListMatrix
mat23 = [[1,2,3],[4,5,6]]
mat32 :: ListMatrix.ListMatrix
mat32 = [[7,8],[9,10],[11,12]]
-- main :: IO ()
-- main =
-- 	let av1 = readVector
-- 	in av1 >>= \a1 ->
--         let av2 = readVector
--         in av2 >>= \a2 ->
--         let mat = av1:av2:[]
--         in foldr (\v a -> printVector v) mat (putStr "")
print2rows :: ListMatrix.ListMatrix -> IO()
print2rows mat = TensorIO.printVector  (head mat) >> TensorIO.printVector  (head $tail mat)
print3rows mat = TensorIO.printVector  (head mat) >> print2rows (tail mat)
main :: IO ()
main = print3rows mat3 >> print3rows (ListMatrix.transpose mat3)
-- main = print $ ListMatrix.determinant mat2

-- main = TensorIO.printVector $ ListVector.listVectorSlice 4 3 vec