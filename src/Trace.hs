{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# language PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE DerivingVia #-}
module Trace where
import Data.VectorSpace (sumV, VectorSpace(..), AdditiveGroup(..))
import Expr
import System.IO (hPutStrLn, stderr)
import GHC.IO (evaluate, unsafePerformIO)
import Sharing ()
import Graph
import Control.Monad (when)
import qualified NodeMap
import Notensor (FullVector(..))
import GHC.Generics (Generic)
import EType (Node(Node), Endpoint (SourceNode, InnerNode), Edge(..))
import BVar.Num(var, backpropNum, AsNum (unAsNum))
import OpenGraph (OpenGraph)
import NodeMap
import Diff (GradOf)
import Back (BackFun(BackFun))
import Data.Semigroup (Sum(Sum, getSum))

newtype R = R { unR :: Integer }
    deriving (Show, Generic)
    deriving Num via Integer

f0 :: Floating a => a -> a
f0 x = sin (2*x)

df :: Floating a => (forall x. Floating x => x -> x) -> a -> a
df f = backpropNum . f . var

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
    type VecBuilder R = Sum R
    sumBuilder = getSum

instance FullVector R where
    identityBuilder = Sum
    negateBuilder = Sum . negateV
    scaleBuilder a = Sum . (a *^)

tracingFunc :: String -> Integer -> BackFun R R
tracingFunc name value = BackFun back
    where back (R x) = unsafePerformIO $ do
            x' <- evaluate x
            let y = value*x'
            hPutStrLn stderr (name ++ "'(" ++ show x' ++ ") -> " ++ show y)
            return (Sum (R (value*x')))

exprToTerm :: FullVector dv => Expr BackFun da dv -> Term BackFun da dv
exprToTerm = Term (BackFun identityBuilder)


testExpr :: IO (Expr BackFun R R)
testExpr = do
    let f = tracingFunc "f" 2
        g = tracingFunc "g" 3
        x0, x1 :: Expr BackFun R R
        x0 = ExprSum [Term f ExprVar]
        x1 = ExprSum [Term g ExprVar]
        x2 = ExprSum [exprToTerm x0, exprToTerm x1]
        x3 = ExprSum [exprToTerm x1, exprToTerm x2]
        x4 = ExprSum [exprToTerm x2, exprToTerm x3]
        x5 = ExprSum [exprToTerm x3, exprToTerm x4]
        x6 = ExprSum [exprToTerm x4, exprToTerm x5]
        x7 = ExprSum [exprToTerm x5, exprToTerm x6]
    --    xs = [x0, x1, x2, x3, x4, x5, x6, x7]
    --traverse_ evaluate xs
    --names <- traverse makeStableName  xs
    --forM_ (zip [0..] names) $ \(i, name) -> putStrLn ("x" ++ show i ++ " " ++ show (hashStableName name))
    return x7

-- >>> testExpr ✕ (R 7)
-- R 56
{-
x :: () -> IO (SomeGraph _ _ _)
x () = do
    g <- runRecoverSharing4 =<< testExpr
    let sm = cvtmap g
    --let NodeMap.SomeSharedExprWithMap smap expr = cvtmap g
-}
{-
_y :: IO R
_y = do
    SharedExprWithMap m' x <- _x()
    let y' = forgetSharing2 (x, m') :: Expr2 R R R R
    return (y' ✕ R 1)
-}
{-
_z :: IO ()
_z = do
    g <- x ()
    case g of
        SomeGraph y' -> do
            let dy' = flipGraph y'
            putStrLn "back"
            ans2 <- evaluate (unVec (dy' ✕ Vec (R 2)))
            print ans2
            return ()
-}
