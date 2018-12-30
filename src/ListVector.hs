module ListVector
    ( Scalar,
        ListVector,
        average,
        -- movingAverage,
        listSlice,
        listVectorSlice,
        (ListVector.+),
        (ListVector..),
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
    let (total_sum, total_count) = foldr (\curr_val (curr_sum,curr_count) -> (curr_sum Prelude.+ curr_val,curr_count Prelude.+ 1) ) (0.0,0) vec
    in total_sum / total_count

-- movingSum :: Double -> (Double,Double) -> ListVector

-- movingSum currentEl (previousSum, previousEl) =
--     (previousSum-previousEl  Prelude.+ currentEl,currentEl)

listSlice :: Int -> Int -> Int -> Int-> [a]  -> [a]
listSlice  sliceStrideConst sliceStrideSoFar sliceStart sliceLen []   = undefined
listSlice  sliceStrideConst sliceStrideSoFar sliceStart sliceLen vec@(element:rest)   =
    if sliceLen <= 0
        then []
        else if sliceStart > 0
            then listSlice sliceStrideConst sliceStrideSoFar (sliceStart-1) sliceLen rest
            else if sliceStrideSoFar > 1
                then listSlice sliceStrideConst (sliceStrideSoFar-1) 0 sliceLen rest
                else element:(listSlice sliceStrideConst sliceStrideConst 0 sliceLen rest )

listVectorSlice  :: Int -> Int -> Int-> ListVector -> ListVector
listVectorSlice stride = ListVector.listSlice stride stride

-- movingAverage :: Int -> ListVector -> ListVector
-- movingAverage 0 vec = []
-- movingAverage window [] = []
-- movingAverage window vec =
--     let windI = fromIntegral window
--     in let firstEl = head vec
--     in let init = windI * firstEl
--     in foldr movingSum (init,firstEl)

(+) :: ListVector -> ListVector -> ListVector
(+) = zipWith (Prelude.+)

(.) :: ListVector -> ListVector -> Scalar
(.) a b = let c = zipWith (*) a b in foldr (Prelude.+) 0.0 c
dotProduct = (ListVector..)

--crossproduct :: ListVector -> ListVector -> ListVector

scalarMultiply :: Scalar -> ListVector -> ListVector
scalarMultiply scal =
    map (scal*)