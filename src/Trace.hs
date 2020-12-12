module Trace where
import Tensor
import Data.VectorSpace (AdditiveGroup(..))
import Expr
import System.IO (hPutStrLn, stderr)
import GHC.IO (evaluate, unsafePerformIO)
import Sharing
import ExprRef
import qualified Debug.Trace

newtype R = R Integer
    deriving Show

instance AdditiveGroup R where
    zeroV = unsafePerformIO $ do
        hPutStrLn stderr "zero"
        return (R 0)
    negateV (R x) = unsafePerformIO $ do
        y <- evaluate (negate x)
        hPutStrLn stderr ("negate " ++ show x ++ " -> " ++ show y)
        return (R (negate x))
    R x ^+^ R y = unsafePerformIO $ do
        x' <- evaluate x
        y' <- evaluate y
        let z = x' + y'
        hPutStrLn stderr (show x' ++ "+" ++ show y' ++ " -> " ++ show z)
        return (R z)

tracingFunc :: String -> Integer -> AFunction R R R R R
tracingFunc name value = AFunction fwd back
    where fwd (R x) = unsafePerformIO $ do
            x' <- evaluate x
            let y = value*x'
            hPutStrLn stderr (name ++ "(" ++ show x' ++ ") -> " ++ show y) 
            return (R (value*x'))
          back (R x) = unsafePerformIO $ do
            x' <- evaluate x
            let y = value*x'
            hPutStrLn stderr (name ++ "'(" ++ show x' ++ ") -> " ++ show y) 
            return (R (value*x'))


testExpr :: () -> Expr R R R R R
testExpr () = x4
    where f = tracingFunc "f" 2
          g = tracingFunc "g" 3
          x0 = Func f Variable
          x1 = Func g Variable
          x2 = Sum [x0, x1]
          x3 = Sum [x1, x2]
          x4 = Sum [x2, x3]

-- >>> testExpr ⊗ (R 7)
-- R 56
_x :: () -> IO (SExpr R R R R R, ExprMap (SExpr R R R))
_x () = runRecoverSharing (testExpr ())

_y :: IO R
_y = do
    x' <- _x()
    let y' = forgetSharing x' :: Expr R R R R R
    return (y' ⊗ (R 1))
