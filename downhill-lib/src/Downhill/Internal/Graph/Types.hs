{-| Types of nodes and edges of the computational graph.

Parameters:

  * @p@ - is parent node; might be 'OpenKey' or 'NodeKey'

  * @e@ - edge type

  * @a@ - type of the initial node of expression

  * @v@ - type of the node.
-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
module Downhill.Internal.Graph.Types
(
  -- * Graph parts
  Edge(..), Endpoint(..), Node(..),
  -- * Linear functions
  BackFun(..), FwdFun(..),
  flipBackFun, flipFwdFun
)
 where

import Downhill.Linear.Expr (BasicVector (VecBuilder))

data Endpoint p a v where
    SourceNode :: Endpoint p a a
    InnerNode :: p v -> Endpoint p a v

data Edge p e a v where
    Edge :: e u v -> Endpoint p a u -> Edge p e a v

{-| Inner node. This does not include initial node. Contains a list
of ingoing edges. -}
data Node p e a v = BasicVector v => Node [Edge p e a v]


-- | Edge type for backward mode evaluation
newtype BackFun u v = BackFun {unBackFun :: v -> VecBuilder u}

-- | Edge type for forward mode evaluation
newtype FwdFun u v = FwdFun {unFwdFun :: u -> VecBuilder v}

flipBackFun :: BackFun u v -> FwdFun v u
flipBackFun (BackFun f) = FwdFun f

flipFwdFun :: FwdFun u v -> BackFun v u
flipFwdFun (FwdFun f) = BackFun f

