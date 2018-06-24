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

transpose :: ListMatrix -> ListMatrix
transpose [] = []
transpose [a] = [a]
transpose a = 

determinant :: ListMatrix -> Scalar

(*) :: ListMatrix -> ListMatrix -> ListMatrix