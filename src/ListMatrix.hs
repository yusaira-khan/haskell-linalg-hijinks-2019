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

(+) :: ListVector -> ListVector -> ListVector
(+) = zipWith (+)