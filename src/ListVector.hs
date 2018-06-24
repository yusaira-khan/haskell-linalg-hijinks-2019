module ListVector
    ( Scalar,
    	ListVector,
    	average,
    	movingAverage,
    	(+),
    	(.)
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
dotProduct = (.)

--crossproduct :: ListVector -> ListVector -> ListVector

scalarMultiply :: Scalar -> Listvector -> ListVector
scalarMultiply scal =
	map (scal*) 