{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# language ScopedTypeVariables #-}
module Graph where
import ExprRef (ExprMap, ExprName, SomeExprWithName(..))
import Sharing(SharedTerm(..), SharedExpr(..), SharedArg(..))
import Tensor(AFunction(..))

import qualified ExprRef as ExprMap

data EdgeFrom u du = FromVariable | FromExpr (ExprName u du)
data EdgeTo v dv = ToFinal | ToExpr (ExprName v dv)
data Edge b u du v dv = Edge (EdgeFrom u du) (EdgeTo v dv) (AFunction b u du v dv)
data SomeEdge b = forall u du v dv. SomeEdge (Edge b u du v dv)

--data EdgeOut v dv = forall u du. EdgeOut (ExprName v dv) 

edgeSource :: SharedArg b a da v dv -> EdgeFrom v dv
edgeSource = \case
    SharedArgVar -> FromVariable
    SharedArgExpr xname -> FromExpr xname

inEdges :: SharedExpr b a da v dv -> EdgeTo v dv -> [SomeEdge b]
inEdges x dst = case x of SharedExprSum xs -> map makeEdge xs
    where makeEdge = \case
            SharedFunAp f arg -> SomeEdge (Edge (edgeSource arg) dst f)

allEdges :: forall b a v da dv. (SharedExpr b a da v dv, ExprMap (SharedExpr b a da)) -> [SomeEdge b]
allEdges (y, env) = inEdges y ToFinal ++ concatMap internalEdges nodes
    where nodes :: [SomeExprWithName (SharedExpr b a da)]
          nodes = ExprMap.toList env
          internalEdges :: SomeExprWithName (SharedExpr b a da) -> [SomeEdge b]
          internalEdges = \case
            SomeExprWithName xname expr -> inEdges expr (ToExpr xname)

flipFrom :: EdgeFrom u du -> EdgeTo du u
flipFrom = \case
    FromVariable -> ToFinal
    FromExpr x -> ToExpr x
flipTo :: EdgeTo u du -> EdgeFrom u du
flipTo = \case
    ToFinal -> FromVariable
    ToExpr x -> FromExpr x

flipEdge :: Edge b u du v dv -> Edge b dv v du u
flipEdge (Edge from to f) = Edge (_flipTo to) (_flipFrom from) (transposeFunc f)
--forgetSharing2 :: 
