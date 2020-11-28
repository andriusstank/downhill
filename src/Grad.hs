{-# LANGUAGE MultiParamTypeClasses #-}
module Grad
where

import Tensor

class TensorProduct dv f du => LinearGradFunc f du dv where

