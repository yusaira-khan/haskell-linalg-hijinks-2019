module ListVector
    ( Scalar,
        ListVector,
        average,
        movingAverage,
        (+),
        (.),
        scalarMultiply
    ) where
import Control.Monad
import Control.Applicative
import Data.List
import Debug.Trace

type Scalar = Double
type ListVector = [Scalar]

average :: ListVector -> Double
average vec =
    let (total_sum, total_count) = foldr (\curr_val (curr_sum,curr_count) -> (curr_sum + curr_val,curr_count+1) ) (0.0,0) vec
    in total_sum / total_count

movingSum :: Double -> Double -> ListVector
movingSum currentEl (previousSum, previousEl) =
    (previousSum-previousEl+currentEl,currentEl)

listSlice :: Int -> Int -> Int -> Int-> [a]  -> [a]
listSlice  sliceStrideConst sliceStrideSoFar sliceStart sliceLen []   = undefinded
listSlice  sliceStrideConst sliceStrideSoFar sliceStart sliceLen vec@(element:rest)   =
    if sliceLen <= 0
        then []
        else if sliceStart > 0
            then vectorSlice sliceStrideConst sliceStrideSoFar (sliceStart-1) sliceLen rest
            else if sliceStrideSoFar > 1
                then vectorSlice sliceStrideConst (sliceStrideSoFar-1) 0 sliceLen rest
                else element:(vectorSlice sliceStrideConst sliceStrideConst 0 sliceLen rest )

listVectorSlice  :: Int  -> Int -> Int -> Int-> ListVector -> ListVector
listVectorSlice stride = ListVector.listSlice stride stride

movingAverage :: Int -> ListVector -> ListVector
movingAverage 0 vec = []
movingAverage window [] = []
movingAverage window vec =
    let windI = fromIntegral window
    in let firstEl = head vec
    in let init = windI * firstEl
    in foldr movingSum (init,firstEl)

(+) :: ListVector -> ListVector -> ListVector
(+) = zipWith (+)

(.) :: ListVector -> ListVector -> Scalar
(.) = foldr (+) 0.0 . zipWith (*)
dotProduct = (ListVector..)

--crossproduct :: ListVector -> ListVector -> ListVector

scalarMultiply :: Scalar -> ListVector -> ListVector
scalarMultiply scal =
    map (scal*)