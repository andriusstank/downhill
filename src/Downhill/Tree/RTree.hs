{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Downhill.Tree.RTree (RegularTree (..), regularTreeLineTree, renderLineTree, LineNode (..), LineTree (..))
where

import Data.List.Singletons (SList (..))
import Data.Singletons (Sing, SingI (..), type (@@), type (~>))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Vinyl (Rec (..))
import Downhill.Tree.HListOf (HListOf (..))
import Downhill.Tree.RegularRecord (
    toRegularRecordFields, RecordFieldNames (..), RegularTreeShape (..), RegularTreeRep, SRegularTreeShape (..), IsRegularRecord (..),
 )
import Downhill.Tree.RegularTree ()
import NumHask.Prelude
import Prelude.Singletons (ConstSym1)

newtype RegularTree (leaf :: k ~> Type) (sh :: RegularTreeShape k) = RegularTree {unRegularTree :: RegularTreeRep leaf @@ sh}

data RegularTree2 (leaf :: k ~> Type) (sh :: RegularTreeShape k) where
    RegularLeafField :: Sing sh -> leaf @@ sh -> RegularTree2 leaf ('RegularTreeShapeLeaf sh)
    RegularSubtreeField :: RegularTreeFields2 leaf sh -> RegularTree2 leaf ('RegularTreeShapeSubtree record)

type RegularTreeUnnamedField ::
    forall k.
    (k ~> Type) ->
    RegularTreeShape k ->
    Type
data RegularTreeUnnamedField (leaf :: k ~> Type) (sh :: RegularTreeShape k) where
    RegularTreeUnnamedField :: forall leaf treeSh. RegularTree2 leaf treeSh -> RegularTreeUnnamedField leaf treeSh

type RegularTreeNamedField ::
    forall k.
    (k ~> Type) ->
    RegularTreeShape k ->
    Type
data RegularTreeNamedField (leaf :: k ~> Type) (sh :: RegularTreeShape k) where
    RegularTreeNamedField :: Text -> RegularTree2 leaf treeSh -> RegularTreeNamedField leaf treeSh

data RegularTreeFields2 (leaf :: k ~> Type) (sh :: [RegularTreeShape k]) where
    RegularTreeTupleFields :: Rec (RegularTreeUnnamedField leaf) fields -> RegularTreeFields2 leaf fields
    RegularTreeNamedFields :: Rec (RegularTreeNamedField leaf) fields -> RegularTreeFields2 leaf fields

regularRecordFieldsToVinyl :: forall leaf sh. Sing sh -> RegularTreeRep leaf @@ sh -> RegularTree2 leaf sh
regularRecordFieldsToVinyl (SRegularTreeShapeLeaf sh) = RegularLeafField sh
regularRecordFieldsToVinyl SRegularTreeShapeSubtree = RegularSubtreeField . goRecord sing
  where
    goRecord ::
        forall record.
        (IsRegularRecord record) =>
        Sing (FieldShapes record) ->
        record leaf ->
        RegularTreeFields2 leaf (FieldShapes record)
    goRecord sh y = case fieldNames @record of
        UnnamedFields -> RegularTreeTupleFields (goTuple sh (toRegularRecordFields y))
        NamedFields names -> RegularTreeNamedFields (goNamed sh names (toRegularRecordFields y))
    goTuple ::
        Sing fields ->
        HListOf (RegularTreeRep leaf) fields ->
        Rec (RegularTreeUnnamedField leaf) fields
    goTuple SNil HNilOf = RNil
    goTuple (SCons shHead shTail) (HConsOf rHead rTail) = RegularTreeUnnamedField (regularRecordFieldsToVinyl shHead rHead) :& goTuple shTail rTail
    goNamed ::
        Sing fields ->
        HListOf (ConstSym1 Text) fields ->
        HListOf (RegularTreeRep leaf) fields ->
        Rec (RegularTreeNamedField leaf) fields
    goNamed SNil HNilOf HNilOf = RNil
    goNamed (SCons shHead shTail) (HConsOf name namesTail) (HConsOf rHead rTail) =
        RegularTreeNamedField name (regularRecordFieldsToVinyl shHead rHead) :& goNamed shTail namesTail rTail

data LineTree
    = LineInline Text
    | LineBlock [LineNode]
    deriving (Eq, Show)

{- | A labelled display node.

Inline children render on the same line as the label. Block children render
below it.
-}
data LineNode = LineNode Text LineTree
    deriving (Eq, Show)

regularTreeLineTree ::
    forall {k} (leaf :: k ~> Type) (sh :: RegularTreeShape k).
    (forall leafSh. Sing leafSh -> leaf @@ leafSh -> LineTree) ->
    Sing sh ->
    RegularTree leaf sh ->
    LineTree
regularTreeLineTree printLeaf sh (RegularTree x) = regularTree (regularRecordFieldsToVinyl sh x)
  where
    regularTree :: forall (sh' :: RegularTreeShape k). RegularTree2 leaf sh' -> LineTree
    regularTree (RegularLeafField leafSh leaf) = printLeaf leafSh leaf
    regularTree (RegularSubtreeField record'') = LineBlock (regularRecordNodes record'')

    regularRecordNodes :: forall sh'. RegularTreeFields2 leaf sh' -> [LineNode]
    regularRecordNodes (RegularTreeTupleFields fields) = tupleNodes 0 fields
    regularRecordNodes (RegularTreeNamedFields fields) = namedNodes fields

    tupleNodes :: forall (fields :: [RegularTreeShape k]). Int -> Rec (RegularTreeUnnamedField leaf) fields -> [LineNode]
    tupleNodes _ RNil = []
    tupleNodes i (RegularTreeUnnamedField field :& fields) = LineNode (Text.pack (show i)) (regularTree field) : tupleNodes (i + 1) fields

    namedNodes :: forall (fields :: [RegularTreeShape k]). Rec (RegularTreeNamedField leaf) fields -> [LineNode]
    namedNodes RNil = []
    namedNodes (RegularTreeNamedField name field :& fields) = LineNode name (regularTree field) : namedNodes fields

renderLineTree :: LineTree -> Text
renderLineTree = Text.intercalate newline . renderTree 0
  where
    newline = Text.pack "\n"
    indent = Text.replicate
    step = Text.pack "  "
    colon = Text.pack ":"
    colonSpace = Text.pack ": "

    renderTree :: Int -> LineTree -> [Text]
    renderTree level (LineInline line) = [indent level step <> line]
    renderTree level (LineBlock nodes) = concatMap (renderNode level) nodes

    renderNode :: Int -> LineNode -> [Text]
    renderNode level (LineNode label (LineInline value)) =
        [indent level step <> label <> colonSpace <> value]
    renderNode level (LineNode label tree@(LineBlock _)) =
        (indent level step <> label <> colon) : renderTree (level + 1) tree
