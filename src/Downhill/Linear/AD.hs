module Downhill.Linear.AD (
    AD (..),
    Bigraph,
    ad,
)
where

import Data.Kind (Type)
import Downhill.Internal.Graph.Bigraph (Bigraph, buildBigraph)
import Downhill.Linear.Builder (HasBuilder (..))
import Downhill.Linear.Expr (BiFun, Term, VecPair (VecPair), idBiFun)
import Downhill.Linear.Fun (DualOf)

newtype AD (a :: Type) (v :: Type)
    = AD
        ( forall x.
          BiFun ('VecPair v (DualOf v)) x ->
          Term ('VecPair a (DualOf a)) x
        )

ad ::
    forall a z.
    ( HasBuilder a
    , HasBuilder z
    , HasBuilder (DualOf z)
    , HasBuilder (DualOf a)
    ) =>
    AD a z -> Bigraph a z
ad (AD f) = buildBigraph [f idBiFun]
