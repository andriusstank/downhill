module Downhill.Tree.FieldNaming (
    FieldNaming (..),
)
where

-- | Whether record fields have names or are identified only by position.
data FieldNaming = Named | Unnamed
