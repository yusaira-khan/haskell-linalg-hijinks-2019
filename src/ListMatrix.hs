{-# LANGUAGE ScopedTypeVariables #-}
module ListMatrix
    ( ListMatrix,
        (ListMatrix.+),
        multiply,
        -- (*),
        determinant,
        ListMatrix.transpose,
        inverseStructure
    ) where

import ListVector
import Control.Monad
import Control.Applicative
import Data.List
import Data.Bool
import Debug.Trace

type ListMatrix = [ListVector.ListVector]


(+) :: ListMatrix -> ListMatrix -> ListMatrix
(+) = zipWith (ListVector.+)

-- todo: handle infinite
transpose :: ListMatrix -> ListMatrix
transpose [] = []
-- transpose [a] = [a]
transpose mat =
    let hs = map head mat
    in let ts = map tail mat
    in if length (head ts) ==0 then [hs] else hs : ListMatrix.transpose ts

listMatrixSlice  :: (Int,Int) -> (Int,Int)-> ListMatrix -> ListMatrix
listMatrixSlice (rowSliceStart,colSliceStart) (rowSliceLen,colSliceLen) mat=
    let slicedcols :: ListMatrix = ListVector.listSlice   colSliceStart colSliceLen mat
    in let f :: (ListVector.ListVector-> ListVector.ListVector) = (ListVector.listVectorSlice rowSliceStart rowSliceLen)
    in map f slicedcols
matrixRowMultiSlice ::  Int -> ListMatrix ->[Int] -> [ListMatrix]
matrixRowMultiSlice  rowLen mat [] = []
matrixRowMultiSlice  rowLen mat rowSliceStart@(cur:next) =
    let slice1 :: (ListVector.ListVector-> ListVector.ListVector) = (ListVector.listVectorSlice 0 cur)
    in let slice2 :: (ListVector.ListVector-> ListVector.ListVector) = (ListVector.listVectorSlice (cur Prelude.+1) (rowLen-1-cur))
    in (zipWith (++) (map slice1 mat) (map slice2 mat)):(matrixRowMultiSlice rowLen mat next)

checkMatrixColsEqualLen::ListMatrix->Bool
-- ^checkMatrixColsEqualLen function checks if all the columns of a matrix are equal size
checkMatrixColsEqualLen _ = False

getValidMatrixTotalRowCol :: ListMatrix -> (Int,Int)
-- ^getValidMatrixRowCol function performs checks to see the validity of the function and returns its number of rows and columns
getValidMatrixTotalRowCol mat =((length $ head mat),length mat)

checkSquareMatrix :: ListMatrix -> Bool
checkSquareMatrix mat = undefined
--- a b c
--- d e f
--- g h i
-- --> [a,d,g],[b,e,h],[c,f,i]

determinant :: ListMatrix -> Scalar
determinant [[a]]  = a
-- determinant [[a,c],[b,d]]  = a Prelude.*d - b Prelude.*c
-- determinant  currentMat@[[a,d,g],[b,e,h],[c,f,i]]  =
--     a Prelude.* determinant [[e,h],[f,i]]  - b Prelude.* determinant [[d,g],[f,i]]  Prelude.+ c Prelude.* determinant [[d,g],[e,h]]
determinant  mat@(firstRow:rest) =
    let (rowNum,colNum) = getValidMatrixTotalRowCol mat
    in let rowsIdxEnumerated = [0..rowNum-1]
    in let signs :: [Scalar] = map (\x->(fromIntegral (x`mod`2)) Prelude.*(-2.0)Prelude.+1.0) rowsIdxEnumerated -- 0-> 1,1->-1,2->1,3->-1 ...
    in let slices = matrixRowMultiSlice rowNum  mat rowsIdxEnumerated -- zipWith ()rowsIdxEnumerated (repeat rest)
    in let partialDeterminant :: ListVector= zipWith3 (\s e m-> s Prelude.* e Prelude.* (determinant m)) signs firstRow slices
    in foldr (Prelude.+) 0 partialDeterminant


inverseStructure :: ListMatrix -> ListMatrix
inverseStructure [[a,c],[b,d]] =
    [[d,-1 Prelude.* c], [-1 Prelude.* b,a]]

mutliplybyvector :: ListVector -> ListMatrix -> ListVector
mutliplybyvector vec_a b =
    map (\vec_b ->
    ((ListVector.. )vec_a vec_b)) b
multiply :: ListMatrix -> ListMatrix -> ListMatrix
multiply  a b=
    map (\vec_a -> mutliplybyvector vec_a b) (ListMatrix.transpose a)
(*)=multiply

scalarMultiply :: Scalar -> ListMatrix -> ListMatrix
scalarMultiply scal m =
    map ( \v ->
    ListVector.scalarMultiply scal v) m
