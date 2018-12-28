import System.IO
import ListVector
import ListMatrix
module Main where

mat::ListMatrix.ListMatrix
mat = [[1,2],[3,4]]

main :: IO ()
main =  do
    putStrLn $ show mat
