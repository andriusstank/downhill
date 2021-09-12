{-| Types of nodes and edges of the computational graph.

Parameters:

  * @p@ - is parent node; might be 'OpenKey' or 'NodeKey'

  * @e@ - edge type

  * @a@ - type of the initial node of expression

  * @v@ - type of the node.
-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
module Downhill.Internal.Graph.Types where

import Downhill.Linear.Expr (BasicVector)

data Endpoint p a v where
    SourceNode :: Endpoint p a a
    InnerNode :: p v -> Endpoint p a v

data Edge p e a v where
    Edge :: e u v -> Endpoint p a u -> Edge p e a v

{-| Inner node. This does not include initial node. -}
data Node p e a v = BasicVector v => Node [Edge p e a v]
