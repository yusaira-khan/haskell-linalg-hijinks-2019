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

movingWindowRight :: Int -> Scalar -> [ListVector] -> [ListVector]
movingWindowRight windowSize currentEl windowEls =
    let lastWindow = if null windowEls then [] else head windowEls
    in let preCurWindow = if length lastWindow < windowSize then lastWindow else init lastWindow
    in (currentEl:preCurWindow):windowEls


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
movingAverage windowLen vec =
    let windows =foldr (movingWindowRight windowLen) [] vec
    --in let l = --[y | x <-[1..],let y= if x<windI then x else windI ]
    in map (\s'->((sum s'))/(fromIntegral (genericLength s'))) windows --zipWith (/) s l

(+) :: ListVector -> ListVector -> ListVector
(+) = zipWith (Prelude.+)

(.) :: ListVector -> ListVector -> Scalar
(.) a b = let c = zipWith (*) a b in sum c
dotProduct = (ListVector..)

--crossproduct :: ListVector -> ListVector -> ListVector

scalarMultiply :: Scalar -> ListVector -> ListVector
scalarMultiply scal =
    map (scal*)

data Kalman = Kalman{
    q :: Scalar, -- process noise variance
    r :: Scalar, -- measurement noise variance
    p :: Scalar, -- estimation error variance
    --k :: Scalar, -- kalman gain
    x :: Scalar -- last value
  } deriving (Show)
    --,Monad

filterKRight :: Scalar -> [Kalman] -> [Kalman]
filterKRight val kals =
    let kal = head kals
    in let _p = p kal Prelude.+ q kal
    in let _k = _p / (_p Prelude.+ r kal)
    in let _x = x kal  Prelude.+ _k * (val - x kal)
    in let __p = (1.0  - _k) * _p
    in Kalman{x=_x,p=__p,q=(q kal),r=(r kal)}:kals

filterKLotsRight :: Kalman -> ListVector -> ListVector
filterKLotsRight kal vals =
    if null vals
        then vals
        else let _kal = Kalman {p = (p kal), r = (r kal), q =(q kal), x=(last vals)}
            in let kals = foldr filterKRight [_kal] vals
            in map (\__kal -> x __kal) kals

kalmanData :: Kalman -> ListVector -> ListVector
kalmanData k l =
    let l' = reverse l in reverse $ filterKLotsRight k l'
----------write kalman filter: use monads for state

convolutionsimple :: ListVector -> ListVector -> ListVector
convolutionsimple  k a =
    let lk = length k
    let la = length a
    in let colsIdxEnumerated = [0..(lk-la)]
    in let slices = map(\s -> listSlice s lk a)
    in let dots = map (\s -> dotProduct s k)
    in dots

correlationsimple :: ListVector -> ListVector -> ListVector
correlationsimple k a =
    let k' = reverse k
    in convolutionsimple k' a

