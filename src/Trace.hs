{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# language PartialTypeSignatures #-}
{- OPTIONS_GHC -Wno-unused-imports -}

module Trace where
import Tensor(TensorProduct((⊗)))
import Data.VectorSpace (sumV, VectorSpace(..), AdditiveGroup(..))
import Expr
import System.IO (hPutStrLn, stderr)
import GHC.IO (evaluate, unsafePerformIO)
import Sharing ()
import Graph
import Control.Monad (when)
import qualified NodeMap
import NodeMap (runRecoverSharing5)
import Expr (Term3)
import Notensor (ProdVector(..), FullVector(..), BasicVector(..), identityFunc, AFunction1(AFunction1))
import GHC.Generics (Generic)
import EType (VectorSum(VectorSum))

newtype R = R { unR :: Integer }
    deriving (Show, Generic)

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

instance VectorSpace R where
    type Scalar R = Integer

instance BasicVector R where
    type VecBuilder R = R
    sumBuilder = sumV

instance ProdVector R where
    zeroBuilder = R 0
    identityBuilder = id

instance FullVector R where
    negateBuilder = negateV
    scaleBuilder a = (a *^)

tracingFunc :: String -> Integer -> AFunction1 R R
tracingFunc name value = AFunction1 back
    where back (R x) = unsafePerformIO $ do
            x' <- evaluate x
            let y = value*x'
            hPutStrLn stderr (name ++ "'(" ++ show x' ++ ") -> " ++ show y) 
            return (R (value*x'))

exprToTerm :: FullVector dv => Expr5 da dv -> Term3 (Expr5 da) da dv
exprToTerm = Func2 identityFunc . ArgExpr


testExpr :: IO (Expr5 R R)
testExpr = do
    let f = tracingFunc "f" 2
        g = tracingFunc "g" 3
        x0, x1 :: Expr5 R R
        x0 = Expr5 (VectorSum [Func2 f ArgVar])
        x1 = Expr5 (VectorSum [Func2 g ArgVar])
        x2 = Expr5 (VectorSum [exprToTerm x0, exprToTerm x1])
        x3 = Expr5 (VectorSum [exprToTerm x1, exprToTerm x2])
        x4 = Expr5 (VectorSum [exprToTerm x2, exprToTerm x3])
        x5 = Expr5 (VectorSum [exprToTerm x3, exprToTerm x4])
        x6 = Expr5 (VectorSum [exprToTerm x4, exprToTerm x5])
        x7 = Expr5 (VectorSum [exprToTerm x5, exprToTerm x6])
    --    xs = [x0, x1, x2, x3, x4, x5, x6, x7]
    --traverse_ evaluate xs
    --names <- traverse makeStableName  xs
    --forM_ (zip [0..] names) $ \(i, name) -> putStrLn ("x" ++ show i ++ " " ++ show (hashStableName name))
    return x7

-- >>> testExpr ⊗ (R 7)
-- R 56
_x :: () -> IO (NodeMap.SomeSharedExprWithMap R R)
_x () = runRecoverSharing5 =<< testExpr

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
    let dy' = flipGraph y'
    putStrLn "back"
    ans2 <- evaluate (R 2 ⊗ dy')
    print ans2
    return ()
