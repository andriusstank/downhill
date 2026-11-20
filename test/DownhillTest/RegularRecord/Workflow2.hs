{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module DownhillTest.RegularRecord.Workflow2 (workflow2Tests) where

import Data.Constraint (Dict (Dict))
import Data.Singletons (Apply, type (@@), type (~>))
import Data.Stream.Infinite qualified as Stream
import Downhill.BVar (BVar, DFun)
import Downhill.BVar qualified as BVar
import Downhill.Linear.Builder (HasBuilder)
import Downhill.Linear.Fun (AsLinearMap (..), DualOf, LinearMap (..), Metric)
import Downhill.Manifold (Grad, Tang)
import Downhill.Opt.GD (AccGdProblem (AccGdProblem), accGd)
import Downhill.Tree.RegularRecord (IsRegularRecord, RecordTree (..), RegularTreeShape (..), adRecordTree)
import Downhill.Tree.Representation (PointwiseMetric (metricDict))
import Downhill.Tree.Vector qualified as Vector
import Downhill.VSpace (CScalar, InnerSpace (..))
import DownhillTest.VSpace.V2 (Cov2 (..), V2 (..))
import GHC.Records (getField)
import NumHask.Prelude
import Test.HUnit (Assertion, assertFailure, (@=?))
import Test.Hspec

newtype ScaleMetric = ScaleMetric Double

instance AsLinearMap ScaleMetric Double Double where
    toLinearMap (ScaleMetric s) = LinearMap (s *) (s *)

instance Metric ScaleMetric Double

{-# ANN CalibrationFrame ("HLint: ignore Use newtype instead of data" :: String) #-}
data CalibrationFrame (leaf :: Vector.ParameterShape ~> Type) = CalibrationFrame
    { translation :: leaf @@ 'Vector.VectorParameter
    }
    deriving stock (Generic)

instance IsRegularRecord CalibrationFrame

data PlanarCalibration (leaf :: Vector.ParameterShape ~> Type) = PlanarCalibration
    { frame :: CalibrationFrame leaf
    , scale :: leaf @@ 'Vector.ScalarParameter
    }
    deriving stock (Generic)

instance IsRegularRecord PlanarCalibration

{-# ANN SensorPoint ("HLint: ignore Use newtype instead of data" :: String) #-}
data SensorPoint (leaf :: Vector.ParameterShape ~> Type) = SensorPoint
    { sensorPosition :: leaf @@ 'Vector.VectorParameter
    }
    deriving stock (Generic)

instance IsRegularRecord SensorPoint

{-# ANN WorldPoint ("HLint: ignore Use newtype instead of data" :: String) #-}
data WorldPoint (leaf :: Vector.ParameterShape ~> Type) = WorldPoint
    { worldPosition :: leaf @@ 'Vector.VectorParameter
    }
    deriving stock (Generic)

instance IsRegularRecord WorldPoint

data CalibrationSample (leaf :: Vector.ParameterShape ~> Type) = CalibrationSample
    { measured :: SensorPoint leaf
    , expected :: WorldPoint leaf
    }
    deriving stock (Generic)

instance IsRegularRecord CalibrationSample

class
    ( leaf ~ Vector.ParameterLeaf v
    , InnerSpace v
    ) =>
    VectorTree v leaf
        | leaf -> v

instance (InnerSpace v) => VectorTree v (Vector.ParameterLeaf v)

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
    (VectorTree v leaf) =>
    PlanarCalibration leaf ->
    SensorPoint leaf ->
    WorldPoint leaf
calibrateSensorPoint calibration point =
    WorldPoint
        $ calibration.scale
        *| point.sensorPosition
        + calibration.frame.translation

worldPointError ::
    (VectorTree v leaf) =>
    WorldPoint leaf ->
    WorldPoint leaf ->
    Scalar v
worldPointError actual expected = residual <.> residual
  where
    residual = actual.worldPosition - expected.worldPosition

calibrationSampleError ::
    (VectorTree v leaf) =>
    PlanarCalibration leaf ->
    CalibrationSample leaf ->
    Scalar v
calibrationSampleError calibration sample =
    worldPointError
        (calibrateSensorPoint calibration sample.measured)
        sample.expected

nominalScaleError ::
    forall v leaf.
    (VectorTree v leaf, V2Constants v) =>
    PlanarCalibration leaf ->
    Scalar v
nominalScaleError calibration = residual * residual
  where
    residual = calibration.scale - constantScalar @v 2

calibrationError ::
    forall v leaf.
    (VectorTree v leaf, V2Constants v) =>
    PlanarCalibration leaf ->
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

calibrationObjective ::
    DFun
        (RecordTree (Vector.ParameterLeaf (V2 Double)) ('RegularTreeShapeSubtree PlanarCalibration))
        Double
calibrationObjective = adRecordTree go
  where
    go :: forall r. PlanarCalibration (Vector.ParameterLeaf (BVar r (V2 Double))) -> BVar r Double
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

calibrationValues :: PlanarCalibration (Vector.ParameterLeaf (V2 Double)) -> (Double, V2 Double)
calibrationValues parameters = (parameters.scale, parameters.frame.translation)

wrapRecord :: record leaf -> RecordTree leaf ('RegularTreeShapeSubtree record)
wrapRecord = RecordTree

unwrapRecord :: RecordTree leaf ('RegularTreeShapeSubtree record) -> record leaf
unwrapRecord = unRecordTree

calibrationAccGd ::
    PlanarCalibration CalibrationMetric ->
    PlanarCalibration (Vector.ParameterLeaf (V2 Double)) ->
    Stream.Stream (PlanarCalibration (Vector.ParameterLeaf (V2 Double)), Double)
calibrationAccGd metric = fmap unwrapFst . accGd problem . wrapRecord
  where
    problem = AccGdProblem calibrationObjective (wrapRecord metric) Nothing 1
    unwrapFst (x, y) = (unwrapRecord x, y)

testPlanarCalibrationWorkflow :: Assertion
testPlanarCalibrationWorkflow = do
    let initial = PlanarCalibration (CalibrationFrame (V2 0 0)) 0
        metric :: PlanarCalibration CalibrationMetric
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

workflow2Tests :: SpecWith ()
workflow2Tests =
    describe "Planar calibration workflow" $ do
        it "calibrates planar scale and translation with scalar and vector leaves" testPlanarCalibrationWorkflow
