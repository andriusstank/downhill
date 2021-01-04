{-# language PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Trace where
import Tensor
    ( indentityFunc, AFunction(AFunction), TensorProduct((⊗)) )
import Data.VectorSpace (AdditiveGroup(..))
import Expr
import System.IO (hPutStrLn, stderr)
import GHC.IO (evaluate, unsafePerformIO)
import qualified Debug.Trace
import Sharing ()
import Graph
import Control.Monad (forM_, forM, when)
import System.Mem.StableName (makeStableName, hashStableName)
import Data.Foldable (traverse_)
import qualified NodeMap
import NodeMap (runRecoverSharing3)
import Data.Coerce (coerce)
import Data.Constraint.Unsafe (Coercible)

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
        when ((x/=0) && (y/=0)) $
            hPutStrLn stderr (show x' ++ "+" ++ show y' ++ " -> " ++ show z)
        return (R z)

tracingFunc :: String -> Integer -> AFunction R R R R
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

exprToTerm :: Expr2 a da v dv -> Term2 a da v dv
exprToTerm = Func2 indentityFunc . ArgExpr


testExpr :: IO (Expr2 R R R R)
testExpr = do
    let f = tracingFunc "f" 2
        g = tracingFunc "g" 3
        x0 = Expr2 (ExprSum [Func2 f ArgVar])
        x1 = Expr2 (ExprSum [Func2 g ArgVar])
        x2 = Expr2 (ExprSum [exprToTerm x0, exprToTerm x1])
        x3 = Expr2 (ExprSum [exprToTerm x1, exprToTerm x2])
        x4 = Expr2 (ExprSum [exprToTerm x2, exprToTerm x3])
        x5 = Expr2 (ExprSum [exprToTerm x3, exprToTerm x4])
        x6 = Expr2 (ExprSum [exprToTerm x4, exprToTerm x5])
        x7 = Expr2 (ExprSum [exprToTerm x5, exprToTerm x6])
    --    xs = [x0, x1, x2, x3, x4, x5, x6, x7]
    --traverse_ evaluate xs
    --names <- traverse makeStableName  xs
    --forM_ (zip [0..] names) $ \(i, name) -> putStrLn ("x" ++ show i ++ " " ++ show (hashStableName name))
    return x7

-- >>> testExpr ⊗ (R 7)
-- R 56
_x :: () -> IO (NodeMap.SomeSharedExprWithMap R R R R)
_x () = runRecoverSharing3 =<< testExpr

{-
_y :: IO R
_y = do
    SharedExprWithMap m' x <- _x()
    let y' = forgetSharing2 (x, m') :: Expr2 R R R R
    return (y' ⊗ R 1)
-}

_z :: IO ()
_z = do
    NodeMap.SomeSharedExprWithMap smap expr <- _x ()
    let y' = convertGraph smap expr
    putStrLn "fwd"
    ans1 <- evaluate (y' ⊗ R 2)
    let dy' = flipGraph y'
    putStrLn "back"
    ans2 <- evaluate (R 2 ⊗ dy')
    print (ans1, ans2)
    return ()
