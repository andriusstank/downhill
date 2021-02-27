{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Grad
where

import Tensor

class Bilinear'' dv f du => LinearGradFunc f du dv where

