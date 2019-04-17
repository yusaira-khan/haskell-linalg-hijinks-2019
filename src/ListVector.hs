module ListVector
    ( Scalar,
        ListVector,
        average,
        movingAverage,
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

movingSumHelper :: Scalar -> (ListVector,Scalar,Scalar) -> (ListVector,Scalar,Scalar)

movingSumHelper currentEl (sumlist,previousSum, previousEl) =
    let s = previousSum-previousEl  Prelude.+ currentEl
    in (s:sumlist,s,currentEl)


listSlice :: Int -> Int-> [a] -> [a]
listSlice sliceStart sliceLen []= []
listSlice sliceStart sliceLen vec@(element:rest)=
    if sliceLen <= 0
        then []
        else if sliceStart > 0
            then listSlice (sliceStart-1) sliceLen rest
            else element:(listSlice 0 (sliceLen-1) rest )

listVectorSlice  :: Int -> Int-> ListVector -> ListVector
listVectorSlice  = ListVector.listSlice

movingAverage :: Int -> ListVector -> ListVector
movingAverage 0 vec = []
movingAverage window [] = []
movingAverage window vec =
    let windI = fromIntegral window
    in let firstEl = head vec
    in let init =  windI * firstEl
    in let (s,_,_) =foldr movingSumHelper ([],init,firstEl) vec
    --in let l = --[y | x <-[1..],let y= if x<windI then x else windI ]
    in s
    --in map (\s'->s'/windI) s --zipWith (/) s l

(+) :: ListVector -> ListVector -> ListVector
(+) = zipWith (Prelude.+)

(.) :: ListVector -> ListVector -> Scalar
(.) a b = let c = zipWith (*) a b in foldr (Prelude.+) 0.0 c
dotProduct = (ListVector..)

--crossproduct :: ListVector -> ListVector -> ListVector

scalarMultiply :: Scalar -> ListVector -> ListVector
scalarMultiply scal =
    map (scal*)