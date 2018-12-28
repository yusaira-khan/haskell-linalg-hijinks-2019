module ListMatrix
    ( ListMatrix,
        (+),
        (*),
        determinant,
        inverse
    ) where

import ListVector
import Control.Monad
import Control.Applicative
import Data.List
import Debug.Trace

type ListMatrix = [ListVector.ListVector]


(+) :: ListMatrix -> ListMatrix -> ListMatrix
(+) = zipWith (+)

-- todo: handle infinite
transpose :: ListMatrix -> ListMatrix
transpose a_none@[]::_ = a_none
transpose a_all@[a]::_ = a_all
transpose mat =
    let hs = map head mat
    in let ts = map tail mat
    in hs :: (transpose ts)


listMatrixSlice  :: (Int,Int) -> (Int,Int) -> (Int,Int(-> ListMatrix -> (Int,Int) -> ListMatrix
listMatrixSlice (rowSliceStrideConst,colSliceStrideConst) (rowSliceStart,colSliceStart) (rowSliceLen,colSliceLen) mat=
    let slicedcols = ListVector.listSlice colSliceStrideConst  colSliceStrideConst colSliceStart colSliceLen mat colSliceStride
    in map slicedcols (ListVector.listVectorSlice rowSliceStrideConst rowSliceStrideConst rowSliceStart rowSliceLen)


--- a b c
--- d e f
--- g h i
----> [a,d,g],[b,e,h],[c,f,i]
determinant :: ListMatrix -> Scalar
determinant [[a]] _ = a
determinant [[a,c],[b,d]] _ = a*d - b*c
determinant  currentMat@[[a,d,g],[b,e,h],[c,f,i]]  =
    a * determinant [[e,h],[f,i]]  - b * determinant [[d,g],[f,i]] + c * determinant [[d,g],[e,h]]


inverseStructure :: ListMatrix -> ListMatrix
inverseStructure [[a,c],[b,d]] =
    [[d,-1*c], [-1*b,a]]

multiply :: ListMatrix -> ListMatrix -> ListMatrix
multiply a b=
    transpose a >>= \vec_a
    b >>= \vec_b
    vec_a ListVector.. vec_b
(*)=multiply

scalarMultiply :: Scalar -> ListMatrix -> ListMatrix
scalarMultiply scal m =
    m >>= \v
    ListVector.scalarMultiply v
