{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module DownhillTest.Tree.Workflow2 (treeWorkflow2Tests) where

import Data.Constraint (Dict (Dict))
import Data.Singletons (Apply, type (@@), type (~>))
import Data.Stream.Infinite qualified as Stream
import Downhill.BVar (BVar, DFun)
import Downhill.BVar qualified as BVar
import Downhill.Linear.Builder (HasBuilder)
import Downhill.Linear.Fun (AsLinearMap (..), DualOf, LinearMap (..), Metric)
import Downhill.Manifold (Grad, Tang)
import Downhill.Opt.GD (AccGdProblem (AccGdProblem), accGd)
import Downhill.Tree.Record (IsRecord)
import Downhill.Tree.Representation (PointwiseMetric (metricDict))
import Downhill.Tree.Tree (Tree (..), TreeOf, TreeShape (..), adTreeRecord)
import Downhill.Tree.Vector qualified as Vector
import Downhill.VSpace (CScalar, InnerSpace (..))
import DownhillTest.VSpace.V2 (Cov2 (..), V2 (..))
import GHC.Records (getField)
import NumHask.Prelude
import Test.HUnit (Assertion, assertFailure, (@=?))
import Test.Hspec (SpecWith, describe, it)

type Leaf :: (TreeShape leafShape ~> Type) -> leafShape -> Type
type Leaf f sh = f @@ 'TreeShapeLeaf sh

type Subtree f record = f @@ 'TreeShapeRecord record
type Subtree :: (TreeShape leafShape ~> Type) -> ((TreeShape leafShape ~> Type) -> Type) -> Type

newtype ScaleMetric = ScaleMetric Double

instance AsLinearMap ScaleMetric Double Double where
    toLinearMap (ScaleMetric s) = LinearMap (s *) (s *)

instance Metric ScaleMetric Double

{-# ANN CalibrationFrame ("HLint: ignore Use newtype instead of data" :: String) #-}
data CalibrationFrame f = CalibrationFrame
    { translation :: Leaf f 'Vector.VectorParameter
    }
    deriving stock (Generic)

instance IsRecord CalibrationFrame

data PlanarCalibration f = PlanarCalibration
    { frame :: Subtree f CalibrationFrame
    , scale :: Leaf f 'Vector.ScalarParameter
    }
    deriving stock (Generic)

instance IsRecord PlanarCalibration

{-# ANN SensorPoint ("HLint: ignore Use newtype instead of data" :: String) #-}
data SensorPoint f = SensorPoint
    { sensorPosition :: Leaf f 'Vector.VectorParameter
    }
    deriving stock (Generic)

instance IsRecord SensorPoint

{-# ANN WorldPoint ("HLint: ignore Use newtype instead of data" :: String) #-}
data WorldPoint f = WorldPoint
    { worldPosition :: Leaf f 'Vector.VectorParameter
    }
    deriving stock (Generic)

instance IsRecord WorldPoint

data CalibrationSample f = CalibrationSample
    { measured :: Subtree f SensorPoint
    , expected :: Subtree f WorldPoint
    }
    deriving stock (Generic)

instance IsRecord CalibrationSample

class
    ( repr ~ TreeOf (Vector.ParameterLeaf v)
    , InnerSpace v
    ) =>
    VectorTree v repr
        | repr -> v

instance (InnerSpace v) => VectorTree v (TreeOf (Vector.ParameterLeaf v))

class (CScalar (V2ConstantScalar v), InnerSpace v) => V2Constants v where
    type V2ConstantScalar v :: Type
    constantV2 :: V2 (V2ConstantScalar v) -> v
    constantScalar :: V2ConstantScalar v -> Scalar v

instance (CScalar a) => V2Constants (V2 a) where
    type V2ConstantScalar (V2 a) = a
    constantV2 = id
    constantScalar = id

instance
    ( InnerSpace (BVar r (V2 a))
    , CScalar a
    , HasBuilder (Tang a)
    , HasBuilder (Grad a)
    , DualOf (Grad a) ~ Tang a
    ) =>
    V2Constants (BVar r (V2 a))
    where
    type V2ConstantScalar (BVar r (V2 a)) = a
    constantV2 = BVar.constant
    constantScalar = BVar.constant

calibrateSensorPoint ::
    (VectorTree v repr) =>
    PlanarCalibration repr ->
    SensorPoint repr ->
    WorldPoint repr
calibrateSensorPoint calibration point =
    WorldPoint
        $ calibration.scale
        *| point.sensorPosition
        + calibration.frame.translation

worldPointError ::
    (VectorTree v repr) =>
    WorldPoint repr ->
    WorldPoint repr ->
    Scalar v
worldPointError actual expected = residual <.> residual
  where
    residual = actual.worldPosition - expected.worldPosition

calibrationSampleError ::
    (VectorTree v repr) =>
    PlanarCalibration repr ->
    CalibrationSample repr ->
    Scalar v
calibrationSampleError calibration sample =
    worldPointError
        (calibrateSensorPoint calibration sample.measured)
        sample.expected

nominalScaleError ::
    forall v repr.
    (VectorTree v repr, V2Constants v) =>
    PlanarCalibration repr ->
    Scalar v
nominalScaleError calibration = residual * residual
  where
    residual = calibration.scale - constantScalar @v 2

calibrationError ::
    forall v repr.
    (VectorTree v repr, V2Constants v) =>
    PlanarCalibration repr ->
    Scalar v
calibrationError parameters =
    calibrationSampleError parameters firstSample
        + calibrationSampleError parameters secondSample
        + nominalScaleError parameters
  where
    firstSample =
        CalibrationSample
            (SensorPoint (constantV2 (V2 1 0)))
            (WorldPoint (constantV2 (V2 3 1)))
    secondSample =
        CalibrationSample
            (SensorPoint (constantV2 (V2 0 1)))
            (WorldPoint (constantV2 (V2 1 3)))

type CalibrationTree =
    Tree
        (Vector.ParameterLeaf (V2 Double))
        ('TreeShapeRecord PlanarCalibration)

calibrationObjective :: DFun CalibrationTree Double
calibrationObjective = adTreeRecord go
  where
    go ::
        forall r.
        PlanarCalibration (TreeOf (Vector.ParameterLeaf (BVar r (V2 Double)))) ->
        BVar r Double
    go = calibrationError

newtype PlanarVectorMetric = PlanarVectorMetric Double

instance AsLinearMap PlanarVectorMetric (Cov2 Double) (V2 Double) where
    toLinearMap (PlanarVectorMetric s) = LinearMap forward forward
      where
        forward (Cov2 x y) = V2 (s * x) (s * y)

instance Metric PlanarVectorMetric (Cov2 Double)

data CalibrationMetric :: Vector.ParameterShape ~> Type

type instance Apply CalibrationMetric 'Vector.ScalarParameter = ScaleMetric
type instance Apply CalibrationMetric 'Vector.VectorParameter = PlanarVectorMetric

instance PointwiseMetric CalibrationMetric (Vector.ParameterLeaf (Cov2 Double)) where
    metricDict Vector.SScalarParameter = Dict
    metricDict Vector.SVectorParameter = Dict

type CalibrationParameters =
    PlanarCalibration (TreeOf (Vector.ParameterLeaf (V2 Double)))

type CalibrationMetricRecord = PlanarCalibration (TreeOf CalibrationMetric)

calibrationValues :: CalibrationParameters -> (Double, V2 Double)
calibrationValues parameters = (parameters.scale, parameters.frame.translation)

wrapRecord :: record (TreeOf leaf) -> Tree leaf ('TreeShapeRecord record)
wrapRecord = Tree

unwrapRecord :: Tree leaf ('TreeShapeRecord record) -> record (TreeOf leaf)
unwrapRecord = unTree

calibrationAccGd ::
    CalibrationMetricRecord ->
    CalibrationParameters ->
    Stream.Stream (CalibrationParameters, Double)
calibrationAccGd metric = fmap unwrapFst . accGd problem . wrapRecord
  where
    problem = AccGdProblem calibrationObjective (wrapRecord metric) Nothing 1
    unwrapFst (x, y) = (unwrapRecord x, y)

testPlanarCalibrationTreeWorkflow :: Assertion
testPlanarCalibrationTreeWorkflow = do
    let initial :: CalibrationParameters
        initial = PlanarCalibration (CalibrationFrame (V2 0 0)) 0
        metric :: CalibrationMetricRecord
        metric =
            PlanarCalibration
                (CalibrationFrame (PlanarVectorMetric (1 / 8)))
                (ScaleMetric (1 / 8))

    case Stream.take 2 (calibrationAccGd metric initial) of
        [(initialPoint, initialError), (calibratedPoint, calibratedError)] -> do
            (0, V2 0 0) @=? calibrationValues initialPoint
            24 @=? initialError
            (2, V2 1 1) @=? calibrationValues calibratedPoint
            0 @=? calibratedError
        states -> assertFailure $ "expected two optimizer states, got " <> show (length states)

treeWorkflow2Tests :: SpecWith ()
treeWorkflow2Tests =
    describe "Planar calibration Tree workflow" $ do
        it "calibrates nested IsRecord trees with scalar and vector leaves" testPlanarCalibrationTreeWorkflow
