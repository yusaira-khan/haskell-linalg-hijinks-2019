import System.IO
type Row = [Double]
type Matrix = [Row]

mat::Matrix
mat = [[1,2],[3,4]]

main :: IO ()
main =  do
    putStrLn $ show mat
