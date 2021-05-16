{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
module Back
( BackFun(..), FwdFun(..), flipBackFun
)
where
import Notensor (FullVector(..))
import Data.VectorSpace (AdditiveGroup(..), VectorSpace(..))
import Expr (Expr (ExprSum), zeroE, Term(Term), BasicVector (VecBuilder))

newtype BackFun u v = BackFun { unBackFun :: v -> VecBuilder u }
newtype FwdFun u v = FwdFun  {unFwdFun :: u -> VecBuilder v }

flipBackFun :: BackFun du dv -> FwdFun dv du
flipBackFun (BackFun f) = FwdFun f

instance FullVector v => AdditiveGroup (Expr BackFun a v) where
    zeroV = zeroE
    negateV x = ExprSum [Term (BackFun negateBuilder) x]
    x ^+^ y = ExprSum [Term (BackFun identityBuilder) x, Term (BackFun identityBuilder) y]
    x ^-^ y = ExprSum [Term (BackFun identityBuilder) x, Term (BackFun negateBuilder) y]

instance FullVector dv => VectorSpace (Expr BackFun da dv) where
    type Scalar (Expr BackFun da dv) = Scalar dv
    a *^ v = ExprSum [Term (BackFun (scaleBuilder a)) v]

sumExpr2 :: FullVector dv => [Expr BackFun da dv] -> Expr BackFun da dv
sumExpr2 xs = ExprSum (wrap <$> xs)
    where wrap x = Term (BackFun identityBuilder) x

