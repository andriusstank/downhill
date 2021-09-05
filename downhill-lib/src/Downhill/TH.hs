{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Downhill.TH
  ( --mkDVar,
    mkDVarC,
    RecordNamer (..),
    DVarOptions (..),
    defaultDVarOptions,
  )
where

import Control.Monad
import Data.AdditiveGroup ((^+^), (^-^))
import qualified Data.Map as Map
import Data.VectorSpace (AdditiveGroup (negateV, zeroV), VectorSpace (Scalar, (*^)))
import Downhill.DVar (BVar (BVar))
import Downhill.Grad
  ( Dual (evalGrad),
    HasGrad (Grad, MScalar, Metric, Tang),
    MetricTensor (MtCovector, MtVector, evalMetric, sqrNorm),
  )
import Downhill.Linear.Expr (BasicVector (VecBuilder, sumBuilder))
import Downhill.Linear.Lift (lift1_sparse)
import GHC.Records (HasField (getField))
import Language.Haskell.TH
  ( Bang (Bang),
    Con (NormalC, RecC),
    Cxt,
    Dec (DataD, InstanceD, NewtypeD, SigD),
    Exp (AppE, ConE, InfixE, VarE),
    Name,
    Pat (ConP, VarP),
    Q,
    SourceStrictness (NoSourceStrictness),
    SourceUnpackedness (NoSourceUnpackedness),
    TyVarBndr (..),
    Type (AppT, ConT, VarT),
    nameBase,
    newName,
  )
import Language.Haskell.TH.Datatype (ConstructorInfo (constructorFields, constructorName, constructorVariant), ConstructorVariant (InfixConstructor, NormalConstructor, RecordConstructor), DatatypeInfo (datatypeCons, datatypeInstTypes, datatypeName, datatypeVariant, datatypeVars), DatatypeVariant (Newtype), TypeSubstitution (applySubstitution), reifyDatatype)
import Language.Haskell.TH.Syntax
  ( BangType,
    Body (NormalB),
    Clause (Clause),
    Dec (FunD, TySynInstD, ValD),
    Exp (AppTypeE),
    TyLit (StrTyLit),
    TySynEqn (TySynEqn),
    Type (ArrowT, EqualityT, LitT, SigT),
    VarBangType,
    mkNameS,
  )

data DatatypeFields
  = NormalFields [Type]
  | RecordFields [(String, Type)]
  deriving (Show)

data DownhillRecord = DownhillRecord
  { ddtTypeConName :: Name,
    ddtDataConName :: Name,
    --ddtFields :: DatatypeFields,
    ddtFieldTypes :: [Type],
    ddtFieldNames :: Maybe [String],
    ddtTypeVars :: [TyVarBndr],
    ddtFieldCount :: Int,
    ddtVariant :: DatatypeVariant
  }
  deriving (Show)

data RecordNamer = RecordNamer
  { typeConNamer :: String -> String,
    dataConNamer :: String -> String,
    fieldNamer :: String -> String
  }

data DVarOptions = DVarOptions
  { optTangNamer :: RecordNamer,
    optGradNamer :: RecordNamer,
    optMetricNamer :: RecordNamer,
    optBuilerNamer :: RecordNamer -- TODO: typo
  }

defaultTangRecordNamer :: RecordNamer
defaultTangRecordNamer =
  RecordNamer
    { typeConNamer = (++ "Tang"),
      dataConNamer = (++ "Tang"),
      fieldNamer = id
    }

defaultGradRecordNamer :: RecordNamer
defaultGradRecordNamer =
  RecordNamer
    { typeConNamer = (++ "Grad"),
      dataConNamer = (++ "Grad"),
      fieldNamer = id
    }

defaultMetricRecordNamer :: RecordNamer
defaultMetricRecordNamer =
  RecordNamer
    { typeConNamer = (++ "Metric"),
      dataConNamer = (++ "Metric"),
      fieldNamer = id
    }

defaultBuilderRecordNamer :: RecordNamer
defaultBuilderRecordNamer =
  RecordNamer
    { typeConNamer = (++ "Builder"),
      dataConNamer = (++ "Builder"),
      fieldNamer = id
    }

defaultDVarOptions :: DVarOptions
defaultDVarOptions =
  DVarOptions
    { optTangNamer = defaultTangRecordNamer,
      optGradNamer = defaultGradRecordNamer,
      optMetricNamer = defaultMetricRecordNamer,
      optBuilerNamer = defaultBuilderRecordNamer
    }

--addIdentity :: DownhillRecord Name -> DownhillRecord

mkConstructor :: DownhillRecord -> Con
mkConstructor record =
  case ddtFieldNames record of
    Nothing ->
      NormalC newConstrName (map mkType (ddtFieldTypes record))
    Just names ->
      RecC newConstrName (zipWith mkRecType names (ddtFieldTypes record))
  where
    newConstrName :: Name
    newConstrName = ddtDataConName record
    mkRecType :: String -> Type -> VarBangType
    mkRecType name type_ =
      ( mkNameS name,
        Bang NoSourceUnpackedness NoSourceStrictness,
        type_
      )
    mkType :: Type -> BangType
    mkType type_ =
      ( Bang NoSourceUnpackedness NoSourceStrictness,
        type_
      )

parseGradConstructor :: Name -> DatatypeInfo -> ConstructorInfo -> [TyVarBndr] -> Q DownhillRecord
parseGradConstructor tyName dinfo cinfo typevars = do
  let types = constructorFields cinfo
      n = length types
  (fieldTypes, fieldNames) <- case constructorVariant cinfo of
    NormalConstructor -> return (types, Nothing)
    InfixConstructor -> return (types, Nothing)
    RecordConstructor fieldNames -> do
      --when (length fieldNames /= n) (fail "parseGradConstructor: length fieldNames /= n")
      --return (RecordFields (zipWith (\name ty -> (nameBase name, ty)) fieldNames types))
      return (types, Just (nameBase <$> fieldNames))
  return
    DownhillRecord
      { ddtTypeConName = tyName,
        ddtDataConName = constructorName cinfo,
        ddtTypeVars = typevars,
        ddtFieldCount = n,
        ddtFieldTypes = fieldTypes,
        ddtFieldNames = fieldNames,
        ddtVariant = datatypeVariant dinfo
      }

parseDownhillRecord :: Name -> DatatypeInfo -> Q (DownhillRecord, ConstructorInfo)
parseDownhillRecord recordName record' = do
  let name = datatypeName record'
  let typevars = datatypeVars record'
      constructors' = datatypeCons record'
  constr' <- case constructors' of
    [] -> fail (show recordName <> " has no data constructors")
    [constr''] -> return constr''
    _ -> fail (show recordName <> " has multiple data constructors")

  r <- parseGradConstructor name record' constr' typevars
  return (r, constr')

elementwiseOp :: DownhillRecord -> Name -> Q Dec
elementwiseOp record func = do
  let n = ddtFieldCount record
      dataConName :: Name
      dataConName = ddtDataConName record
  xs <- replicateM n (newName "x")
  ys <- replicateM n (newName "y")
  let fieldOp :: Name -> Name -> Exp
      fieldOp x y = InfixE (Just (VarE x)) (VarE func) (Just (VarE y))
      resultFields :: [Exp]
      resultFields = zipWith fieldOp xs ys
      leftPat = ConP dataConName (map VarP xs)
      rightPat = ConP dataConName (map VarP ys)
      rhs :: Exp
      rhs = foldl AppE (ConE dataConName) resultFields
      dec =
        FunD
          func
          [ Clause
              [leftPat, rightPat]
              (NormalB rhs)
              []
          ]
  return dec

elementwiseValue :: DownhillRecord -> Name -> Q Dec
elementwiseValue record func = do
  let n = ddtFieldCount record
      dataConName :: Name
      dataConName = ddtDataConName record
      rhs :: Exp
      rhs = foldl AppE (ConE dataConName) (replicate n (VarE 'zeroV))
      dec = ValD (VarP func) (NormalB rhs) []
  return dec

elementwiseFunc :: DownhillRecord -> Name -> Q Dec
elementwiseFunc record func = do
  let n = ddtFieldCount record
      dataConName :: Name
      dataConName = ddtDataConName record
      rhsConName = ddtDataConName record
  xs <- case ddtFieldNames record of
    Nothing -> replicateM n (newName "x")
    Just names -> traverse newName names
  let fieldOp :: Name -> Exp
      fieldOp = AppE (VarE func) . VarE
      resultFields :: [Exp]
      resultFields = map fieldOp xs
      leftPat = ConP dataConName (map VarP xs)
      rhs :: Exp
      rhs = foldl AppE (ConE rhsConName) resultFields
      dec =
        FunD
          func
          [ Clause
              [leftPat]
              (NormalB rhs)
              []
          ]
  return dec

mkClassInstance :: Name -> Cxt -> DownhillRecord -> [Type] -> [Dec] -> Q [Dec]
mkClassInstance className cxt record instVars decs = do
  let recordType = ConT (ddtTypeConName record)
      ihead = AppT (ConT className) (foldl AppT recordType instVars)
  return [InstanceD Nothing cxt ihead decs]

mkSemigroupInstance :: Cxt -> DownhillRecord -> [Type] -> Q [Dec]
mkSemigroupInstance cxt record instVars = do
  dec <- elementwiseOp record '(<>)
  mkClassInstance ''Semigroup cxt record instVars [dec]

mkAdditiveGroupInstance :: Cxt -> DownhillRecord -> [Type] -> Q [Dec]
mkAdditiveGroupInstance cxt record instVars = do
  zeroVDec <- elementwiseValue record 'zeroV
  negateDec <- elementwiseFunc record 'negateV
  plusDec <- elementwiseOp record '(^+^)
  minusDec <- elementwiseOp record '(^-^)
  let decs =
        [ zeroVDec,
          negateDec,
          plusDec,
          minusDec
        ]
  mkClassInstance ''AdditiveGroup cxt record instVars decs

mkVectorSpaceInstance :: DownhillRecord -> Type -> Cxt -> [Type] -> Q [Dec]
mkVectorSpaceInstance record scalarType cxt instVars = do
  let n = ddtFieldCount record
      dataConName :: Name
      dataConName = ddtDataConName record
  xs <- case ddtFieldNames record of
    Nothing -> replicateM n (newName "x")
    Just names -> traverse newName names

  lhsName <- newName "s"
  let rightPat = ConP (ddtDataConName record) (map VarP xs)
      recordType = foldl AppT (ConT (ddtTypeConName record)) instVars
      mulField :: Name -> Exp
      mulField y = InfixE (Just (VarE lhsName)) (VarE '(*^)) (Just (VarE y))
      rhsMulV :: Exp
      rhsMulV = foldl AppE (ConE dataConName) (map mulField xs)
  let vmulDec =
        FunD
          '(*^)
          [ Clause
              [VarP lhsName, rightPat]
              (NormalB rhsMulV)
              []
          ]
      scalarTypeDec =
        TySynInstD
          ( TySynEqn
              Nothing
              (AppT (ConT ''Scalar) recordType)
              scalarType
          )
      decs = [scalarTypeDec, vmulDec]
  mkClassInstance ''VectorSpace cxt record instVars decs

mkBasicVectorInstance :: DownhillRecord -> RecordNamer -> Cxt -> [Type] -> Q [Dec]
mkBasicVectorInstance vectorRecord builderNamer cxt instVars = do
  sumBuilderDec <- mkSumBuilder
  mkClassInstance ''BasicVector cxt vectorRecord instVars [vecbuilderDec, sumBuilderDec]
  where
    n = ddtFieldCount vectorRecord
    --vectorRecord :: DownhillRecord
    --vectorRecord = mapDdt (Identity . dvtVector) record
    builderRecord = renameDownhillRecordBuilder builderNamer vectorRecord

    mkSumBuilder :: Q Dec
    mkSumBuilder = do
      builders <- replicateM n (newName "x")
      let pat :: Pat
          pat = ConP (ddtDataConName builderRecord) (map VarP builders)
          rhs :: Exp
          rhs =
            foldl
              AppE
              (ConE (ddtDataConName vectorRecord))
              [AppE (VarE 'sumBuilder) (VarE x) | x <- builders]
      return $
        FunD
          'sumBuilder
          [ Clause [ConP 'Nothing []] (NormalB (VarE 'zeroV)) [],
            Clause [ConP 'Just [pat]] (NormalB rhs) []
          ]

    vecbuilderDec =
      TySynInstD
        ( TySynEqn
            Nothing
            (AppT (ConT ''VecBuilder) vectorType)
            (AppT (ConT ''Maybe) builderType)
        )
      where
        vectorType = foldl AppT (ConT (ddtTypeConName vectorRecord)) instVars
        builderType = foldl AppT (ConT (ddtTypeConName builderRecord)) instVars

sumVExpr :: [Exp] -> Exp
sumVExpr = \case
  [] -> VarE 'zeroV
  exps -> foldl1 (zipExpInfix '(^+^)) exps
  where
    zipExpInfix :: Name -> Exp -> Exp -> Exp
    zipExpInfix f x y = InfixE (Just x) (VarE f) (Just y)

mkDualInstance ::
  DownhillRecord ->
  DownhillRecord ->
  Type ->
  Cxt ->
  [Type] ->
  Q [Dec]
mkDualInstance tangRecord gradRecord scalarType cxt instVars = do
  when (ddtFieldCount tangRecord /= ddtFieldCount gradRecord) $
    fail "mkDualInstance: ddtFieldCount tangRecord /= ddtFieldCount gradRecord"
  scalarTypeName <- newName "s"
  mkClassDec (VarT scalarTypeName)
  where
    n = ddtFieldCount tangRecord
    --tangRecord = mapDdt (Identity . dvtVector . dtTang) record
    --gradRecord = mapDdt (Identity . dvtVector . dtGrad) record

    -- instance (cxt, AdditiveGroup s, s ~ scalarType) => AdditiveGroup (Record a1 … an) where
    --   …
    mkClassDec :: Type -> Q [Dec]
    mkClassDec scalarVar = do
      evalGradDec <- mkEvalGradDec
      return [InstanceD Nothing (cxt ++ newConstraints) ihead [evalGradDec]]
      where
        -- Dual s (RecordTang a1 … an) (RecordGrad a1 … an)
        ihead :: Type
        ihead = ConT ''Dual `AppT` scalarVar `AppT` vecType `AppT` gradType
          where
            vecType = foldl AppT (ConT $ ddtTypeConName tangRecord) instVars
            gradType = foldl AppT (ConT $ ddtTypeConName gradRecord) instVars
        newConstraints :: Cxt
        newConstraints =
          [ -- AdditiveGroup s
            AppT (ConT ''AdditiveGroup) scalarVar,
            -- s ~ scalarType
            AppT (AppT EqualityT scalarVar) scalarType
          ]

        -- evalGrad (RecordGrad x1 … xn) (RecordTang y1 … yn) = evalGrad x1 y1 ^+^ … ^+^ evalGrad xn yn
        mkEvalGradDec :: Q Dec
        mkEvalGradDec = do
          xs <- replicateM n (newName "x")
          ys <- replicateM n (newName "y")
          let leftPat = ConP (ddtDataConName gradRecord) (map VarP xs)
              rightPat = ConP (ddtDataConName tangRecord) (map VarP ys)
              -- terms = [evalGrad x1 y1, …, evalGrad xn yn]
              terms :: [Exp]
              terms = zipWith evalGradExp xs ys
                where
                  evalGradExp :: Name -> Name -> Exp
                  evalGradExp x y = VarE 'evalGrad `AppE` VarE x `AppE` VarE y
              rhs = sumVExpr terms
          return $
            FunD
              'evalGrad
              [ Clause
                  [leftPat, rightPat]
                  (NormalB rhs)
                  []
              ]

mkMetricInstance ::
  DownhillRecord ->
  DownhillRecord ->
  DownhillRecord ->
  Type ->
  Cxt ->
  [Type] ->
  Q [Dec]
mkMetricInstance metricRecord tangRecord gradRecord scalarType cxt instVars = do
  scalarTypeName <- newName "s"
  mkClassDec (VarT scalarTypeName)
  where
    -- instance (ctx, s ~ scalarType) => MetricTensor s (RecordMetric a1 … an) where
    --   …
    mkClassDec :: Type -> Q [Dec]
    mkClassDec scalarVar = do
      let newConstraints =
            [ -- s ~ scalarType
              AppT (AppT EqualityT scalarVar) scalarType
            ]
          -- MetricTensor s (RecordMetric a1 … an)
          ihead = ConT ''MetricTensor `AppT` scalarVar `AppT` metricType
      evalMetricDec <- mkEvalMetric
      sqrNormDec <- mkSqrNorm
      return
        [ InstanceD
            Nothing
            (cxt ++ newConstraints)
            ihead
            [vectypeDec, covectorTypeDec, evalMetricDec, sqrNormDec]
        ]
      where
        vectorType :: Type
        vectorType = foldl AppT (ConT $ ddtTypeConName tangRecord) instVars
        covectorType :: Type
        covectorType = foldl AppT (ConT $ ddtTypeConName gradRecord) instVars
        metricType :: Type
        metricType = foldl AppT (ConT $ ddtTypeConName metricRecord) instVars
        -- type MtVector (RecordMetric a1 … an) = RecordTang a1 … an
        vectypeDec =
          TySynInstD
            ( TySynEqn
                Nothing
                (AppT (ConT ''MtVector) metricType)
                vectorType
            )
        -- type MtCovector (RecordMetric a1 … an) = RecordGrad a1 … an
        covectorTypeDec =
          TySynInstD
            ( TySynEqn
                Nothing
                (AppT (ConT ''MtCovector) metricType)
                covectorType
            )

        mkEvalMetric :: Q Dec
        mkEvalMetric = do
          let n = ddtFieldCount metricRecord
          xs <- replicateM n (newName "m")
          ys <- replicateM n (newName "dv")
          let leftPat, rightPat :: Pat
              leftPat = ConP (ddtDataConName metricRecord) (map VarP xs)
              rightPat = ConP (ddtDataConName gradRecord) (map VarP ys)
              terms :: [Exp]
              terms = zipWith evalGradExp xs ys
                where
                  evalGradExp :: Name -> Name -> Exp
                  evalGradExp x y = VarE 'evalMetric `AppE` VarE x `AppE` VarE y
              rhs =
                foldl
                  AppE
                  (ConE (ddtDataConName tangRecord))
                  terms
          return $
            FunD
              'evalMetric
              [ Clause
                  [leftPat, rightPat]
                  (NormalB rhs)
                  []
              ]

        mkSqrNorm :: Q Dec
        mkSqrNorm = do
          let n = ddtFieldCount metricRecord
          xs <- replicateM n (newName "m")
          ys <- replicateM n (newName "dv")
          let leftPat, rightPat :: Pat
              leftPat = ConP (ddtDataConName metricRecord) (map VarP xs)
              rightPat = ConP (ddtDataConName gradRecord) (map VarP ys)
              terms :: [Exp]
              terms = zipWith evalSqrtNorm xs ys
                where
                  evalSqrtNorm :: Name -> Name -> Exp
                  evalSqrtNorm x y = VarE 'sqrNorm `AppE` VarE x `AppE` VarE y
              rhs = sumVExpr terms
          return $
            FunD
              'sqrNorm
              [ Clause
                  [leftPat, rightPat]
                  (NormalB rhs)
                  []
              ]

mkRecord :: DownhillRecord -> Q [Dec]
mkRecord record = do
  let newConstr = mkConstructor record
  let newRecordName = ddtTypeConName record
  let dataType = case ddtVariant record of
        Newtype -> NewtypeD [] newRecordName (ddtTypeVars record) Nothing newConstr []
        _ -> DataD [] newRecordName (ddtTypeVars record) Nothing [newConstr] []
  return [dataType]

renameTypeS :: (String -> String) -> Name -> Name
renameTypeS f = mkNameS . f . nameBase

data FieldInfo = FieldInfo
  { fiName :: String,
    fiIndex :: Int,
    fiType :: Type
  }

mkGetField ::
  DownhillRecord ->
  DownhillRecord ->
  Cxt ->
  [Type] ->
  FieldInfo ->
  Q [Dec]
mkGetField pointRecord gradBuilderRecord cxt instVars field = do
  rName <- newName "r"
  xName <- newName "x"
  dxName <- newName "dx"
  goName <- newName "go"
  dxdaName <- newName "dx_da"
  let rhsFieldList :: [Exp]
      rhsFieldList =
        replicate (fiIndex field) (VarE 'mempty)
          ++ [VarE dxdaName]
          ++ replicate (n - fiIndex field - 1) (VarE 'mempty)
      -- rhs = MyRecordGradBuilder mempty … mempty dx_da_a6SX mempty … mempty
      rhs :: Exp
      rhs = foldl AppE (ConE (ddtDataConName gradBuilderRecord)) rhsFieldList
  return
    [ InstanceD
        Nothing
        cxt
        ( AppT
            ( AppT
                (AppT (ConT ''HasField) (LitT (StrTyLit (fiName field))))
                (AppT (AppT (ConT ''BVar) (VarT rName)) pointType)
            )
            (AppT (AppT (ConT ''BVar) (VarT rName)) (fiType field))
        )
        [ FunD
            'getField
            [ Clause
                [ConP 'BVar [VarP xName, VarP dxName]]
                ( NormalB
                    ( AppE
                        ( AppE
                            (ConE 'BVar)
                            (AppE (AppTypeE (VarE 'getField) (LitT (StrTyLit (fiName field)))) (VarE xName))
                        )
                        (AppE (AppE (VarE 'lift1_sparse) (VarE goName)) (VarE dxName))
                    )
                )
                [ SigD
                    goName
                    ( AppT
                        ( AppT
                            ArrowT
                            ( ConT ''VecBuilder
                                `AppT` AppT (ConT ''Grad) (fiType field)
                            )
                        )
                        (ConT ''Maybe `AppT` gradBuilderType)
                    ),
                  FunD
                    goName
                    [ Clause
                        [VarP dxdaName]
                        ( NormalB
                            ( AppE
                                (ConE 'Just)
                                rhs
                            )
                        )
                        []
                    ]
                ]
            ]
        ]
    ]
  where
    n = ddtFieldCount pointRecord
    applyVars :: Type -> Type
    applyVars x = foldl AppT x instVars
    pointType :: Type
    pointType = applyVars (ConT $ ddtTypeConName pointRecord)
    gradBuilderType = applyVars (ConT $ ddtTypeConName gradBuilderRecord)

renameDownhillRecordBuilder :: RecordNamer -> DownhillRecord -> DownhillRecord
renameDownhillRecordBuilder builderNamer record =
  DownhillRecord
    { ddtTypeConName = renameTypeS (typeConNamer builderNamer) (ddtTypeConName record),
      ddtDataConName = renameTypeS (dataConNamer builderNamer) (ddtDataConName record),
      ddtTypeVars = ddtTypeVars record,
      ddtFieldCount = ddtFieldCount record,
      ddtFieldTypes = AppT (ConT ''VecBuilder) <$> ddtFieldTypes record,
      ddtFieldNames = fmap (fmap (fieldNamer builderNamer)) (ddtFieldNames record),
      ddtVariant = ddtVariant record
    }

mkVec :: Cxt -> [Type] -> Type -> DownhillRecord -> RecordNamer -> Q [Dec]
mkVec cxt instVars scalarType vectorType builderNamer = do
  let --vectorType, builderType :: DownhillRecord
      --vectorType = mapDdt (Identity . dvtVector) tangVector
      --builderType = mapDdt (Identity . dvtBuilder) tangVector
      builderType = renameDownhillRecordBuilder builderNamer vectorType
  tangDec <- mkRecord vectorType
  tangBuilderDec <- mkRecord builderType
  tangSemigroup <- mkSemigroupInstance cxt builderType instVars
  tangInst <- mkBasicVectorInstance vectorType builderNamer cxt instVars
  additiveTang <- mkAdditiveGroupInstance cxt vectorType instVars
  vspaceTang <- mkVectorSpaceInstance vectorType scalarType cxt instVars
  return
    ( concat
        [ tangDec,
          tangBuilderDec,
          tangInst,
          tangSemigroup,
          additiveTang,
          vspaceTang
        ]
    )

renameDownhillRecordTang :: DVarOptions -> DownhillRecord -> DownhillRecord
renameDownhillRecordTang options record =
  DownhillRecord
    { ddtTypeConName = renameTypeS (typeConNamer namer) (ddtTypeConName record),
      ddtDataConName = renameTypeS (dataConNamer namer) (ddtDataConName record),
      ddtTypeVars = ddtTypeVars record,
      ddtFieldCount = ddtFieldCount record,
      ddtFieldTypes = AppT (ConT ''Tang) <$> ddtFieldTypes record,
      ddtFieldNames = fmap (fmap (fieldNamer (optTangNamer options))) (ddtFieldNames record),
      ddtVariant = ddtVariant record
    }
  where
    namer = optTangNamer options

renameDownhillRecordGrad :: DVarOptions -> DownhillRecord -> DownhillRecord
renameDownhillRecordGrad options record =
  DownhillRecord
    { ddtTypeConName = renameTypeS (typeConNamer namer) (ddtTypeConName record),
      ddtDataConName = renameTypeS (dataConNamer namer) (ddtDataConName record),
      ddtTypeVars = ddtTypeVars record,
      ddtFieldCount = ddtFieldCount record,
      ddtFieldTypes = AppT (ConT ''Grad) <$> ddtFieldTypes record,
      ddtFieldNames = fmap (fmap (fieldNamer (optTangNamer options))) (ddtFieldNames record),
      ddtVariant = ddtVariant record
    }
  where
    namer = optGradNamer options

renameDownhillRecordMetric :: DVarOptions -> DownhillRecord -> DownhillRecord
renameDownhillRecordMetric options record =
  DownhillRecord
    { ddtTypeConName = renameTypeS (typeConNamer namer) (ddtTypeConName record),
      ddtDataConName = renameTypeS (dataConNamer namer) (ddtDataConName record),
      ddtTypeVars = ddtTypeVars record,
      ddtFieldCount = ddtFieldCount record,
      ddtFieldTypes = AppT (ConT ''Metric) <$> ddtFieldTypes record,
      ddtFieldNames = fmap (fieldNamer (optMetricNamer options)) <$> ddtFieldNames record,
      ddtVariant = ddtVariant record
    }
  where
    namer = optMetricNamer options

mkDVar'' ::
  Cxt ->
  DownhillRecord ->
  DVarOptions ->
  Type ->
  [Type] ->
  ConstructorInfo ->
  Q [Dec]
mkDVar'' cxt pointRecord options scalarType instVars substitutedCInfo = do
  let tangRecord = renameDownhillRecordTang options pointRecord
      gradRecord = renameDownhillRecordGrad options pointRecord
      metricRecord = renameDownhillRecordMetric options pointRecord

  tangDecs <- mkVec cxt instVars scalarType tangRecord (optBuilerNamer options)
  gradDecs <- mkVec cxt instVars scalarType gradRecord (optBuilerNamer options)

  metricDec <- mkRecord metricRecord
  additiveMetric <- mkAdditiveGroupInstance cxt metricRecord instVars
  vspaceMetric <- mkVectorSpaceInstance metricRecord scalarType cxt instVars
  dualInstance <- mkDualInstance tangRecord gradRecord scalarType cxt instVars
  metricInstance <- mkMetricInstance metricRecord tangRecord gradRecord scalarType cxt instVars

  hasFieldInstance <- case ddtFieldNames pointRecord of
    Nothing -> return []
    Just names ->
      let info :: Int -> String -> Type -> FieldInfo
          info index name stype_ = FieldInfo name index stype_
          substitutedFields = constructorFields substitutedCInfo
          fields :: [FieldInfo]
          fields = zipWith3 info [0 ..] names substitutedFields
       in concat <$> traverse (mkGetField pointRecord (renameDownhillRecordBuilder (optBuilerNamer options) gradRecord) cxt instVars) fields

  let decs =
        [ tangDecs,
          gradDecs,
          additiveMetric,
          vspaceMetric,
          dualInstance,
          metricDec,
          metricInstance,
          hasFieldInstance
        ]
  return (concat decs)

parseRecordType :: Type -> [Type] -> Q (Name, [Type])
parseRecordType type_ vars = case type_ of
  AppT inner typeVar -> parseRecordType inner (typeVar : vars)
  ConT recordName -> return (recordName, vars)
  _ -> fail "Expected (T a1 ... an) in constraint"

mkDVarC1 :: DVarOptions -> Dec -> Q [Dec]
mkDVarC1 options = \case
  InstanceD mayOverlap cxt type_ decs -> do
    case mayOverlap of
      Just _ -> fail "Overlapping instances not implemented"
      _ -> return ()
    case type_ of
      -- AppT (ConT Downhill.Grad.HasGrad) (AppT (ConT Main.MyRecord1) (VarT a_1))
      AppT (ConT hasgradCtx) recordInConstraintType -> do
        when (hasgradCtx /= ''HasGrad) $
          fail $ "Constraint must be `HasGrad`, got " ++ show hasgradCtx
        (recordName, instVars) <- parseRecordType recordInConstraintType []
        record' <- reifyDatatype recordName
        (parsedRecord, cinfo) <- parseDownhillRecord recordName record'
        recordTypeVarNames <- do
          let getName x = do
                --getName (SigT (VarT y) _) <- y
                let SigT (VarT y) _ = x
                --ConT y <- return x
                return y
          traverse getName (datatypeInstTypes record')
        -- We have two sets of type variables: one in record definition (as in `data MyRecord a b c = ...`)
        -- and another one in instance head (`instance HasGrad (MyRecord a' b' c')). We need
        -- those from instance head for HasField instances.
        let substPairs = zip recordTypeVarNames instVars
            substitutedRecord = applySubstitution (Map.fromList substPairs) cinfo
        {-
        liftIO $ do
          putStrLn " === Record ==="
          print record'
          putStrLn " === Constraint ==="
          print recordInConstraintType
          putStrLn " === Substitution ==="
          --print substPairs
          print substitutedRecord
        -}
        --dvar <- mkDVar' options cxt recordName instVars
        scalarType <- case decs of
          [] -> fail "`HasGrad` instance has no declarations"
          [dec1] -> case dec1 of
            TySynInstD (TySynEqn _ (AppT (ConT scalarName) _) scalarType) -> do
              when (scalarName /= ''MScalar) $
                fail ("Expected `Scalar` equation, got " ++ show scalarName)
              return scalarType
            _ -> fail "HasGrad instance must contain `Scalar ... = ...` declaration"
          _ -> fail "`HasGrad` has multiple declarations"

        dvar <- mkDVar'' cxt parsedRecord options scalarType instVars substitutedRecord

        let tangName = ddtTypeConName (renameDownhillRecordTang options parsedRecord)
            gradName = ddtTypeConName (renameDownhillRecordGrad options parsedRecord)
            metricName = ddtTypeConName (renameDownhillRecordMetric options parsedRecord)
            tangTypeDec =
              TySynInstD
                ( TySynEqn
                    Nothing
                    (AppT (ConT ''Tang) recordInConstraintType)
                    (foldl AppT (ConT tangName) instVars)
                )
            gradTypeDec =
              TySynInstD
                ( TySynEqn
                    Nothing
                    (AppT (ConT ''Grad) recordInConstraintType)
                    (foldl AppT (ConT gradName) instVars)
                )
            metricTypeDec =
              TySynInstD
                ( TySynEqn
                    Nothing
                    (AppT (ConT ''Metric) recordInConstraintType)
                    (foldl AppT (ConT metricName) instVars)
                )

            hasgradInstance =
              InstanceD
                Nothing
                cxt
                type_
                ( decs
                    ++ [ tangTypeDec,
                         gradTypeDec,
                         metricTypeDec
                       ]
                )
        return $ dvar ++ [hasgradInstance]
      _ -> fail "Instance head is not a constraint"
  _ -> fail "Expected instance declaration"

mkDVarC :: DVarOptions -> Q [Dec] -> Q [Dec]
mkDVarC options decs = concat <$> (traverse (mkDVarC1 options) =<< decs)
