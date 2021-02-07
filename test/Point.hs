module Point where
import Diff

data Vector a = Vector { vectorX :: a, vectorY :: a }
data Point a = Point { pointX :: a, pointY :: a}

--vectorX' :: BVar (Vector a) da dv -> BVar a da dv
--vectorX' = 