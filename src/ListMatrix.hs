{-# LANGUAGE ScopedTypeVariables #-}
module ListMatrix
    ( ListMatrix,
        (ListMatrix.+),
        -- (*),
        determinant,
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
-- transpose :: ListMatrix -> ListMatrix
-- transpose [] = []
-- transpose a_all@[a] = a_all
-- transpose mat =
--     let hs = map head mat
--     in let ts = map tail mat
--     in hs :: transpose ts

listMatrixSlice  :: (Int,Int) -> (Int,Int) -> (Int,Int)-> ListMatrix -> ListMatrix
listMatrixSlice (rowSliceStrideConst,colSliceStrideConst) (rowSliceStart,colSliceStart) (rowSliceLen,colSliceLen) mat=
    let slicedcols :: ListMatrix = ListVector.listSlice colSliceStrideConst  colSliceStrideConst colSliceStart colSliceLen mat
    in let f :: (ListVector.ListVector-> ListVector.ListVector) = (ListVector.listVectorSlice rowSliceStrideConst rowSliceStart rowSliceLen)
    in map f slicedcols

checkMatrixColsEqualLen::ListMatrix->Bool
-- ^checkMatrixColsEqualLen function checks if all the columns of a matrix are equal size
checkMatrixColsEqualLen _ = False

getValidMatrixRowCol :: ListMatrix -> (Int,Int)
-- ^getValidMatrixRowCol function performs checks to see the validity of the function and returns its number of rows and columns
getValidMatrixRowCol mat =(0,0)

checkSquareMatrix :: ListMatrix -> Bool
checkSquareMatrix mat = undefined
--- a b c
--- d e f
--- g h i
-- --> [a,d,g],[b,e,h],[c,f,i]
determinant :: ListMatrix -> Scalar
determinant [[a]]  = a
determinant [[a,c],[b,d]]  = a Prelude.*d - b Prelude.*c
determinant  currentMat@[[a,d,g],[b,e,h],[c,f,i]]  =
    a Prelude.* determinant [[e,h],[f,i]]  - b Prelude.* determinant [[d,g],[f,i]]  Prelude.+ c Prelude.* determinant [[d,g],[e,h]]


inverseStructure :: ListMatrix -> ListMatrix
inverseStructure [[a,c],[b,d]] =
    [[d,-1 Prelude.*c], [-1 Prelude.*b,a]]

-- multiply :: ListMatrix -> ListMatrix -> ListMatrix
-- multiply a b=
--     ListMatrix.transpose a >>= \vec_a ->
--     b >>= \vec_b ->
--     vec_a ListVector.. vec_b
-- (*)=multiply

scalarMultiply :: Scalar -> ListMatrix -> ListMatrix
scalarMultiply scal m =
    map ( \v ->
    ListVector.scalarMultiply scal v) m
