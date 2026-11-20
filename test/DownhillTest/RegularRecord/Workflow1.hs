{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module DownhillTest.RegularRecord.Workflow1 (workflow1Tests) where

import Data.Constraint (Dict (Dict))
import Data.Singletons (Apply, Sing, SingI (sing), type (@@), type (~>))
import qualified Data.Stream.Infinite as Stream
import Downhill.BVar (BVar, DFun)
import Downhill.Linear.Fun (AsLinearMap (..), DualOf, LinearMap (..), Metric, (<*|), (|*>))
import Downhill.Manifold (Manifold, Tang)
import Downhill.Opt.GD (AccGdProblem (AccGdProblem), accGd)
import Downhill.Tree.BVar (BVarRepr (..))
import Downhill.Tree.RegularRecord (IsRegularRecord, RecordTree (..), RegularTreeShape (..), adRecordTree)
import Downhill.Tree.Representation (
    AllConstrained (allConstrained),
    PointwiseManifold (manifoldDict),
    PointwiseMetric (metricDict),
    PointwiseVSpace (vspaceDict),
    ReprDualOf,
    ReprScalar,
    ReprTang,
 )
import Downhill.VSpace (InnerSpaceMetric (InnerSpaceMetric), VSpace)
import DownhillTest.VSpace.V2 (Cov2 (..), V2 (..))
import GHC.Records (getField)
import NumHask.Prelude
import Prelude.Singletons (ConstSym1)
import Test.Hspec
import Test.HUnit (Assertion, assertFailure, (@=?))

data ParameterShape = ParameterScalar

data SParameterShape (sh :: ParameterShape) where
    SParameterScalar :: SParameterShape 'ParameterScalar

type instance Sing = SParameterShape

instance SingI 'ParameterScalar where
    sing = SParameterScalar

data ParameterLeaf (a :: Type) :: ParameterShape ~> Type

type instance Apply (ParameterLeaf a) 'ParameterScalar = a

type instance ReprScalar (ParameterLeaf a) = Scalar a
type instance ReprTang (ParameterLeaf a) = ParameterLeaf (Tang a)
type instance ReprDualOf (ParameterLeaf a) = ParameterLeaf (DualOf a)

instance (c a) => AllConstrained c (ParameterLeaf a) where
    allConstrained SParameterScalar = Dict

instance (VSpace a) => PointwiseVSpace (ParameterLeaf a) where
    vspaceDict SParameterScalar = Dict

instance (Manifold a) => PointwiseManifold (ParameterLeaf a) where
    manifoldDict SParameterScalar = Dict

instance BVarRepr (ParameterLeaf a) where
    type ReprBVar r (ParameterLeaf a) = ParameterLeaf (BVar r a)
    splitBVarField SParameterScalar = id
    joinBVarField SParameterScalar = id

data InitialState (leaf :: ParameterShape ~> Type) = InitialState
    { position :: leaf @@ 'ParameterScalar
    , velocity :: leaf @@ 'ParameterScalar
    }
    deriving stock (Generic)

instance IsRegularRecord InitialState

data Parameters (leaf :: ParameterShape ~> Type) = Parameters
    { initialState :: InitialState leaf
    , mass :: leaf @@ 'ParameterScalar
    , targetPosition :: leaf @@ 'ParameterScalar
    , targetVelocity :: leaf @@ 'ParameterScalar
    }
    deriving stock (Generic)

instance IsRegularRecord Parameters

type WorkflowTree = RecordTree (ParameterLeaf Double) ('RegularTreeShapeSubtree Parameters)

workflowValues :: WorkflowTree -> (Double, Double, Double, Double, Double)
workflowValues (RecordTree record) =
    ( record.initialState.position
    , record.initialState.velocity
    , record.mass
    , record.targetPosition
    , record.targetVelocity
    )

quadraticLoss ::
    (Scalar a ~ a, Additive a, Multiplicative a) =>
    Parameters (ParameterLeaf a) ->
    a
quadraticLoss record =
    let a = record.initialState.position
        b = record.initialState.velocity
        c = record.mass
        d = record.targetPosition
        e = record.targetVelocity
     in a * a + b * b + c * c + d * d + e * e

workflowObjective :: DFun WorkflowTree Double
workflowObjective = adRecordTree quadraticLoss

testWorkflow :: Assertion
testWorkflow = do
    let initial = RecordTree $ Parameters (InitialState 1 2) 3 4 5
        problem =
            AccGdProblem
                workflowObjective
                InnerSpaceMetric
                Nothing
                0.5

    case Stream.take 2 (accGd problem initial) of
        [(initialPoint, initialLoss), (minimumPoint, minimumLoss)] -> do
            (1, 2, 3, 4, 5) @=? workflowValues initialPoint
            55 @=? initialLoss
            (0, 0, 0, 0, 0) @=? workflowValues minimumPoint
            0 @=? minimumLoss
        states -> assertFailure $ "expected two optimizer states, got " <> show (length states)

newtype ScaleMetric = ScaleMetric Double

instance AsLinearMap ScaleMetric Double Double where
    toLinearMap (ScaleMetric s) = LinearMap (s *) (s *)

instance Metric ScaleMetric Double

instance PointwiseMetric (ParameterLeaf ScaleMetric) (ParameterLeaf Double) where
    metricDict SParameterScalar = Dict

testMetricTree :: Assertion
testMetricTree = do
    let metric :: RecordTree (ParameterLeaf ScaleMetric) ('RegularTreeShapeSubtree Parameters)
        metric = RecordTree $ Parameters (InitialState (ScaleMetric 1) (ScaleMetric 2)) (ScaleMetric 0) (ScaleMetric (-1)) (ScaleMetric 3)
        initial :: WorkflowTree
        initial = RecordTree $ Parameters (InitialState 1 2) 3 4 5
        problem = AccGdProblem workflowObjective metric Nothing 0.5
    (1, 4, 0, -4, 15) @=? workflowValues (metric <*| initial)
    (1, 4, 0, -4, 15) @=? workflowValues (initial |*> metric)
    case Stream.take 2 (accGd problem initial) of
        [_, (updated, _)] -> (0, -2, 3, 8, -10) @=? workflowValues updated
        states -> assertFailure $ "expected two optimizer states, got " <> show (length states)

testDistinctDualMetricTree :: Assertion
testDistinctDualMetricTree = do
    let metric :: RecordTree (ConstSym1 InnerSpaceMetric) ('RegularTreeShapeLeaf 'ParameterScalar)
        metric = RecordTree InnerSpaceMetric
        covector :: RecordTree (ConstSym1 (Cov2 Double)) ('RegularTreeShapeLeaf 'ParameterScalar)
        covector = RecordTree (Cov2 2 3)
        RecordTree forward = metric <*| covector
        RecordTree backward = covector |*> metric
    V2 2 3 @=? forward
    V2 2 3 @=? backward

workflow1Tests :: SpecWith ()
workflow1Tests =
    describe "Workflow" $ do
        it "differentiates and optimizes a leaf-polymorphic regular record" testWorkflow
        it "applies nested metric trees in both directions and in optimization" testMetricTree
        it "maps distinct cotangent types to tangent types in both directions" testDistinctDualMetricTree
