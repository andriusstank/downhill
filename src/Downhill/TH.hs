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
    RecordNamer,
    DVarOptions,
    defaultDVarOptions,
  )
where

import Control.Monad
import Data.AdditiveGroup ((^+^), (^-^))
import Data.Functor.Identity (Identity (Identity, runIdentity))
import Data.VectorSpace (AdditiveGroup (negateV, zeroV), VectorSpace ((*^)))
import qualified Data.VectorSpace as VectorSpace
import Downhill.Grad
  ( Dual (evalGrad),
    HasGrad (Grad, Metric, Scalar, Tang),
    MetricTensor (MTCovector, MTVector, evalMetric, sqrNorm),
  )
import Downhill.Linear.Expr (BasicVector (VecBuilder, sumBuilder))
import Language.Haskell.TH
  ( Bang (Bang),
    Con (NormalC, RecC),
    Cxt,
    Dec (DataD, InstanceD),
    Exp (AppE, ConE, InfixE, VarE),
    Info (TyConI),
    Name,
    Pat (ConP, VarP),
    Q,
    SourceStrictness (NoSourceStrictness),
    SourceUnpackedness (NoSourceUnpackedness),
    TyVarBndr (..),
    Type (AppT, ConT, VarT),
    nameBase,
    newName,
    reify,
  )
import Language.Haskell.TH.Syntax
  ( BangType,
    Body (NormalB),
    Clause (Clause),
    Dec (FunD, TySynInstD, ValD),
    TySynEqn (TySynEqn),
    Type (EqualityT),
    VarBangType,
    mkNameS,
  )

data DatatypeFields f
  = NormalFields [f Type]
  | RecordFields [f (String, Type)]

data DownhillVectorType a = DownhillVectorType
  { dvtVector :: a,
    dvtBuilder :: a
  }

data DownhillTypes a = DownhillTypes
  { dtPoint :: a,
    dtTang :: DownhillVectorType a,
    dtGrad :: DownhillVectorType a,
    dtMetric :: a
  }

data DownhillDataType f = DownhillDataType
  { ddtTypeConName :: f Name,
    ddtDataConName :: f Name,
    ddtTypeVars :: [TyVarBndr],
    ddtFieldCount :: Int,
    ddtFields :: DatatypeFields f
  }

deriving instance Show a => Show (DownhillVectorType a)

deriving instance Show a => Show (DownhillTypes a)

deriving instance Show (DatatypeFields DownhillTypes)

deriving instance Show (DownhillDataType DownhillTypes)

mapDdt :: (forall a. f a -> g a) -> DownhillDataType f -> DownhillDataType g
mapDdt f x =
  DownhillDataType
    { ddtTypeConName = f (ddtTypeConName x),
      ddtDataConName = f (ddtDataConName x),
      ddtTypeVars = ddtTypeVars x,
      ddtFieldCount = ddtFieldCount x,
      ddtFields = case ddtFields x of
        NormalFields fields -> NormalFields (f <$> fields)
        RecordFields fields -> RecordFields (f <$> fields)
    }

data RecordNamer = RecordNamer
  { typeConNamer :: String -> String,
    dataConNamer :: String -> String,
    fieldNamer :: String -> String
  }

data DVarOptions = DVarOptions
  { optTangNamer :: RecordNamer,
    optGradNamer :: RecordNamer,
    optMetricNamer :: RecordNamer,
    optBuilerNamer :: RecordNamer
  }

defaultTangRecordNamer :: RecordNamer
defaultTangRecordNamer =
  RecordNamer
    { typeConNamer = (++ "TangT"),
      dataConNamer = (++ "TangD"),
      fieldNamer = id
    }

defaultGradRecordNamer :: RecordNamer
defaultGradRecordNamer =
  RecordNamer
    { typeConNamer = (++ "GradT"),
      dataConNamer = (++ "GradD"),
      fieldNamer = id
    }

defaultMetricRecordNamer :: RecordNamer
defaultMetricRecordNamer =
  RecordNamer
    { typeConNamer = (++ "MetricT"),
      dataConNamer = (++ "MetricD"),
      fieldNamer = id
    }

defaultBuilderRecordNamer :: RecordNamer
defaultBuilderRecordNamer =
  RecordNamer
    { typeConNamer = (++ "BuilderT"),
      dataConNamer = (++ "BuilderD"),
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

mkConstructor :: DownhillDataType Identity -> Con
mkConstructor record =
  case ddtFields record of
    NormalFields types ->
      NormalC newConstrName (map mkType types)
    RecordFields types ->
      RecC newConstrName (map mkRecType types)
  where
    newConstrName :: Name
    newConstrName = runIdentity (ddtDataConName record)
    mkRecType :: Identity (String, Type) -> VarBangType
    mkRecType (Identity (name, type_)) =
      ( mkNameS name,
        Bang NoSourceUnpackedness NoSourceStrictness,
        type_
      )
    mkType :: Identity Type -> BangType
    mkType (Identity type_) =
      ( Bang NoSourceUnpackedness NoSourceStrictness,
        type_
      )

parseGradConstructor :: Name -> Con -> [TyVarBndr] -> Q (DownhillDataType Identity)
parseGradConstructor tyName c typevars = case c of
  NormalC name types ->
    return $
      DownhillDataType
        { ddtTypeConName = Identity tyName,
          ddtDataConName = Identity name,
          ddtTypeVars = typevars,
          ddtFieldCount = length types,
          ddtFields = NormalFields (map (Identity . snd) types)
        }
  RecC name types ->
    return $
      DownhillDataType
        { ddtTypeConName = Identity tyName,
          ddtDataConName = Identity name,
          ddtTypeVars = typevars,
          ddtFieldCount = length types,
          ddtFields = RecordFields [Identity (nameBase fname, ty) | (fname, _, ty) <- types]
        }
  _ -> fail ("Unsupported constructor type: " ++ show c)

parseDownhillDataType :: Name -> Q (DownhillDataType Identity)
parseDownhillDataType recordName = do
  record <- reify recordName
  recordT <- case record of
    TyConI recordT' -> return recordT'
    _ -> fail (show recordName ++ " is not a type")

  (name, constructors, typevars) <- do
    case recordT of
      DataD _cxt name tyvars _kind constructors _deriv -> do
        --unless (null tyvars) $
        --  fail (show recordName ++ " has type variables")
        return (name, constructors, tyvars)
      _ -> fail (show recordName ++ " is not a data type")
  constr <- case constructors of
    [] -> fail (show recordName <> " has no data constructors")
    [constr'] -> return constr'
    _ -> fail (show recordName <> " has multiple data constructors")
  parseGradConstructor name constr typevars

elementwiseOp :: DownhillDataType Identity -> Name -> Q Dec
elementwiseOp record func = do
  let n = ddtFieldCount record
      dataConName :: Name
      dataConName = runIdentity (ddtDataConName record)
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

elementwiseValue :: DownhillDataType Identity -> Name -> Q Dec
elementwiseValue record func = do
  let n = ddtFieldCount record
      dataConName :: Name
      dataConName = runIdentity (ddtDataConName record)
      rhs :: Exp
      rhs = foldl AppE (ConE dataConName) (replicate n (VarE 'zeroV))
      dec = ValD (VarP func) (NormalB rhs) []
  return dec

data RecordFunc a = RecordFunc
  { rfIn :: a,
    rfOut :: a
  }

elementwiseFunc :: DownhillDataType RecordFunc -> Name -> Q Dec
elementwiseFunc record func = do
  let n = ddtFieldCount record
      dataConName :: Name
      dataConName = rfIn (ddtDataConName record)
      rhsConName = rfOut (ddtDataConName record)
  xs <- replicateM n (newName "x")
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

elementwiseFunc' :: DownhillDataType Identity -> Name -> Q Dec
elementwiseFunc' r = elementwiseFunc (mapDdt (\(Identity x) -> RecordFunc x x) r)

mkClassInstance :: Name -> Cxt -> DownhillDataType Identity -> [Type] -> [Dec] -> Q [Dec]
mkClassInstance className cxt record instVars decs = do
  let recordType = ConT (runIdentity (ddtTypeConName record))
      ihead = AppT (ConT className) (foldl AppT recordType instVars)
  return [InstanceD Nothing cxt ihead decs]

mkSemigroupInstance :: Cxt -> DownhillDataType Identity -> [Type] -> Q [Dec]
mkSemigroupInstance cxt record instVars = do
  dec <- elementwiseOp record '(<>)
  mkClassInstance ''Semigroup cxt record instVars [dec]

mkAdditiveGroupInstance :: Cxt -> DownhillDataType Identity -> [Type] -> Q [Dec]
mkAdditiveGroupInstance cxt record instVars = do
  zeroVDec <- elementwiseValue record 'zeroV
  negateDec <- elementwiseFunc' record 'negateV
  plusDec <- elementwiseOp record '(^+^)
  minusDec <- elementwiseOp record '(^-^)
  let decs =
        [ zeroVDec,
          negateDec,
          plusDec,
          minusDec
        ]
  mkClassInstance ''AdditiveGroup cxt record instVars decs

mkVectorSpaceInstance :: DownhillDataType Identity -> Type -> Cxt -> [Type] -> Q [Dec]
mkVectorSpaceInstance record scalarType cxt instVars = do
  let n = ddtFieldCount record
      dataConName :: Name
      dataConName = runIdentity (ddtDataConName record)
  xs <- replicateM n (newName "x")
  lhsName <- newName "s"
  let rightPat = ConP (runIdentity (ddtDataConName record)) (map VarP xs)
      recordType = foldl AppT (ConT (runIdentity (ddtTypeConName record))) instVars
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
              (AppT (ConT ''VectorSpace.Scalar) recordType)
              scalarType
          )
      decs = [scalarTypeDec, vmulDec]
  mkClassInstance ''VectorSpace cxt record instVars decs

mkBasicVectorInstance :: DownhillDataType DownhillVectorType -> Cxt -> [Type] -> Q [Dec]
mkBasicVectorInstance record cxt instVars = do
  sumBuilderDec <- mkSumBuilder
  mkClassInstance ''BasicVector cxt vectorRecord instVars [vecbuilderDec, sumBuilderDec]
  where
    n = ddtFieldCount record
    vectorRecord :: DownhillDataType Identity
    vectorRecord = mapDdt (Identity . dvtVector) record

    mkSumBuilder :: Q Dec
    mkSumBuilder = do
      builders <- replicateM n (newName "x")
      let pat :: Pat
          pat = ConP (dvtBuilder (ddtDataConName record)) (map VarP builders)
          rhs :: Exp
          rhs =
            foldl
              AppE
              (ConE (dvtVector (ddtDataConName record)))
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
        vectorType = foldl AppT (ConT (dvtVector (ddtTypeConName record))) instVars
        builderType = foldl AppT (ConT (dvtBuilder (ddtTypeConName record))) instVars

mkDualInstance :: DownhillDataType DownhillTypes -> Type -> Cxt -> [Type] -> Q [Dec]
mkDualInstance record scalarType cxt instVars = do
  scalarTypeName <- newName "s"
  mkClassDec (VarT scalarTypeName)
  where
    n = ddtFieldCount record

    -- instance (cxt, AdditiveGroup s, s ~ scalarType) => AdditiveGroup (Record a1 … an) where
    mkClassDec :: Type -> Q [Dec]
    mkClassDec scalarVar = do
      evalGradDec <- mkEvalGradDec
      return [InstanceD Nothing (cxt ++ newConstraints) ihead [evalGradDec]]
      where
        -- Dual s (RecordTang a1 … an) (RecordGrad a1 … an)
        ihead :: Type
        ihead = ConT ''Dual `AppT` scalarVar `AppT` vecType `AppT` gradType
          where
            vecType = foldl AppT (ConT . dvtVector . dtTang $ ddtTypeConName record) instVars
            gradType = foldl AppT (ConT . dvtVector . dtGrad $ ddtTypeConName record) instVars
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
          let leftPat = ConP (dvtVector . dtGrad $ ddtDataConName record) (map VarP xs)
              rightPat = ConP (dvtVector . dtTang $ ddtDataConName record) (map VarP ys)
              -- terms = [evalGrad x1 y1, …, evalGrad xn yn]
              terms :: [Exp]
              terms = zipWith evalGradExp xs ys
                where
                  evalGradExp :: Name -> Name -> Exp
                  evalGradExp x y = VarE 'evalGrad `AppE` VarE x `AppE` VarE y
              rhs = sumExpr terms
          return $
            FunD
              'evalGrad
              [ Clause
                  [leftPat, rightPat]
                  (NormalB rhs)
                  []
              ]
          where
            sumExpr :: [Exp] -> Exp
            sumExpr = \case
              [] -> VarE 'zeroV
              exps -> foldl1 (zipExpInfix '(^+^)) exps
              where
                zipExpInfix :: Name -> Exp -> Exp -> Exp
                zipExpInfix f x y = InfixE (Just x) (VarE f) (Just y)

mkMetricInstance :: DownhillDataType DownhillTypes -> Type -> Cxt -> [Type] -> Q [Dec]
mkMetricInstance record scalarType cxt instVars = do
  let n = ddtFieldCount record
  scalarTypeName <- newName "s"
  xs <- replicateM n (newName "x")
  let vectorType = foldl AppT (ConT . dvtVector . dtTang $ ddtTypeConName record) instVars
      gradType = foldl AppT (ConT . dvtVector . dtGrad $ ddtTypeConName record) instVars
      metricType = foldl AppT (ConT . dtMetric $ ddtTypeConName record) instVars
  --ihead' = AppT (AppT (AppT (ConT ''MetricTensor) (VarT scalarTypeName)) vectorType) gradType
  let vectorConName = dvtVector (dtTang (ddtDataConName record))
      gradConName = dvtVector (dtGrad (ddtDataConName record))
  let vectorPat :: Pat
      vectorPat = ConP vectorConName (map VarP xs)
      {-
      rhs :: Exp
      rhs =
        foldl
          AppE
          (ConE dataConName)
          [AppE (VarE 'sumBuilder) (VarE x) | x <- builders]
      -}
      scalarConstraint = AppT (AppT EqualityT (VarT scalarTypeName)) scalarType
      ihead' = ConT ''MetricTensor `AppT` VarT scalarTypeName `AppT` metricType
  let vectypeDec =
        TySynInstD
          ( TySynEqn
              Nothing
              (AppT (ConT ''MTVector) metricType)
              vectorType
          )
      covectypeDec =
        TySynInstD
          ( TySynEqn
              Nothing
              (AppT (ConT ''MTCovector) metricType)
              gradType
          )
  {-sumBuilderDec =
    FunD
      'sumBuilder
      [ Clause [ConP 'Nothing []] (NormalB (VarE 'zeroV)) [],
        Clause [ConP 'Just [pat]] (NormalB rhs) []
      ]-}
  return
    [ InstanceD
        Nothing
        (cxt ++ [scalarConstraint])
        ihead'
        [vectypeDec, covectypeDec]
    ]

mkRecord :: DownhillDataType Identity -> Q [Dec]
mkRecord record = do
  let newConstr = mkConstructor record
  let newRecordName = runIdentity (ddtTypeConName record)
  let dataType = DataD [] newRecordName (ddtTypeVars record) Nothing [newConstr] []
  return [dataType]

renameTypeS :: (String -> String) -> Name -> Name
renameTypeS f = mkNameS . f . nameBase

renameDownhillDataType' :: DVarOptions -> DownhillDataType Identity -> DownhillDataType DownhillTypes
renameDownhillDataType' options record =
  DownhillDataType
    { ddtTypeConName = renameCon typeConNamer (ddtTypeConName record),
      ddtDataConName = renameCon dataConNamer (ddtDataConName record),
      ddtTypeVars = ddtTypeVars record,
      ddtFieldCount = ddtFieldCount record,
      ddtFields = renameFields (ddtFields record)
    }
  where
    renameFields :: DatatypeFields Identity -> DatatypeFields DownhillTypes
    renameFields = \case
      NormalFields fs -> NormalFields (types <$> fs)
      RecordFields fs -> RecordFields (fields <$> fs)

    renameCon :: (RecordNamer -> String -> String) -> Identity Name -> DownhillTypes Name
    renameCon fieldSelector (Identity x) =
      DownhillTypes
        { dtPoint = x,
          dtTang = renameVector' (renameName optTangNamer x),
          dtGrad = renameVector' (renameName optGradNamer x),
          dtMetric = renameName optMetricNamer x
        }
      where
        renameVector' :: Name -> DownhillVectorType Name
        renameVector' name = DownhillVectorType name builderNames
          where
            builderNames = renameName optBuilerNamer name
        renameName :: (DVarOptions -> RecordNamer) -> Name -> Name
        renameName namer name = renameTypeS (fieldSelector (namer options)) name

    mkVectorTypes :: Type -> DownhillVectorType Type
    mkVectorTypes t =
      DownhillVectorType
        { dvtVector = t,
          dvtBuilder = AppT (ConT ''VecBuilder) t
        }
    types :: Identity Type -> DownhillTypes Type
    types (Identity t) =
      DownhillTypes
        { dtPoint = t,
          dtTang = mkVectorTypes (AppT (ConT ''Tang) t),
          dtGrad = mkVectorTypes (AppT (ConT ''Grad) t),
          dtMetric = AppT (ConT ''Metric) t
        }

    mkVectorFields :: (String, Type) -> DownhillVectorType (String, Type)
    mkVectorFields (name, t) =
      DownhillVectorType
        { dvtVector = (name, t),
          dvtBuilder = (fieldNamer (optBuilerNamer options) name, AppT (ConT ''VecBuilder) t)
        }
    fields :: Identity (String, Type) -> DownhillTypes (String, Type)
    fields (Identity (name, t)) =
      DownhillTypes
        { dtPoint = (name, t),
          dtTang = mkVectorFields (fieldNamer (optTangNamer options) name, AppT (ConT ''Tang) t),
          dtGrad = mkVectorFields (fieldNamer (optGradNamer options) name, AppT (ConT ''Grad) t),
          dtMetric = (fieldNamer (optMetricNamer options) name, AppT (ConT ''Metric) t)
        }

mkDVar'' :: Cxt -> DownhillDataType DownhillTypes -> Type -> [Type] -> Q [Dec]
mkDVar'' cxt downhillTypes scalarType instVars = do
  let tangVector = mapDdt dtTang downhillTypes
      gradVector = mapDdt dtGrad downhillTypes
      metric = mapDdt (Identity . dtMetric) downhillTypes

  tangDec <- mkRecord (mapDdt (Identity . dvtVector) tangVector)
  gradDec <- mkRecord (mapDdt (Identity . dvtVector) gradVector)
  metricDec <- mkRecord metric
  tangBuilderDec <- mkRecord (mapDdt (Identity . dvtBuilder) tangVector)
  gradBuilderDec <- mkRecord (mapDdt (Identity . dvtBuilder) gradVector)
  tangSemigroup <- mkSemigroupInstance cxt (mapDdt (Identity . dvtBuilder) tangVector) instVars
  gradSemigroup <- mkSemigroupInstance cxt (mapDdt (Identity . dvtBuilder) gradVector) instVars
  tangInst <- mkBasicVectorInstance (mapDdt dtTang downhillTypes) cxt instVars
  gradInst <- mkBasicVectorInstance (mapDdt dtGrad downhillTypes) cxt instVars
  additiveTang <- mkAdditiveGroupInstance cxt (mapDdt (Identity . dvtVector) tangVector) instVars
  additiveGrad <- mkAdditiveGroupInstance cxt (mapDdt (Identity . dvtVector) gradVector) instVars
  vspaceTang <- mkVectorSpaceInstance (mapDdt (Identity . dvtVector) tangVector) scalarType cxt instVars
  vspaceGrad <- mkVectorSpaceInstance (mapDdt (Identity . dvtVector) gradVector) scalarType cxt instVars
  dualInstance <- mkDualInstance downhillTypes scalarType cxt instVars
  metricInstance <- mkMetricInstance downhillTypes scalarType cxt instVars

  let decs =
        [ tangDec,
          gradDec,
          tangBuilderDec,
          gradBuilderDec,
          tangSemigroup,
          gradSemigroup,
          additiveTang,
          additiveGrad,
          vspaceTang,
          vspaceGrad,
          tangInst,
          gradInst,
          dualInstance,
          metricDec,
          metricInstance
        ]
  return (concat decs)

{-

mkDVar' :: DVarOptions -> Cxt -> Name -> [Type] -> Q [Dec]
mkDVar' options cxt recordName instVars = do
  record <- parseDownhillDataType recordName
  let downhillTypes = renameDownhillDataType' options record
  mkDVar'' options cxt downhillTypes instVars

mkDVar :: DVarOptions -> Name -> Q [Dec]
mkDVar opts typeName = mkDVar' opts [] typeName []

-}

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
      AppT (ConT hasgradCtx) recordType -> do
        when (hasgradCtx /= ''HasGrad) $
          fail $ "Constraint must be `HasGrad`, got " ++ show hasgradCtx
        (recordName, instVars) <- parseRecordType recordType []
        --dvar <- mkDVar' options cxt recordName instVars
        parsedRecord <- parseDownhillDataType recordName
        let downhillTypes = renameDownhillDataType' options parsedRecord
        scalarType <- case decs of
          [] -> fail "`HasGrad` instance has no declarations"
          [dec1] -> case dec1 of
            TySynInstD (TySynEqn _ (AppT (ConT scalarName) _) scalarType) -> do
              when (scalarName /= ''Scalar) $ do
                fail ("Expected `Scalar` equation, got " ++ show scalarName)
              return scalarType
            _ -> fail "HasGrad instance must contain `Scalar ... = ...` declaration"
          _ -> fail "`HasGrad` has multiple declarations"

        dvar <- mkDVar'' cxt downhillTypes scalarType instVars

        let tangName = dvtVector . dtTang $ ddtTypeConName downhillTypes
            gradName = dvtVector . dtGrad $ ddtTypeConName downhillTypes
            metricName = dtMetric $ ddtTypeConName downhillTypes
            tangTypeDec =
              TySynInstD
                ( TySynEqn
                    Nothing
                    (AppT (ConT ''Tang) recordType)
                    (foldl AppT (ConT tangName) instVars)
                )
            gradTypeDec =
              TySynInstD
                ( TySynEqn
                    Nothing
                    (AppT (ConT ''Grad) recordType)
                    (foldl AppT (ConT gradName) instVars)
                )
            metricTypeDec =
              TySynInstD
                ( TySynEqn
                    Nothing
                    (AppT (ConT ''Metric) recordType)
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

{-
InstanceD Nothing
[AppT (ConT Downhill.Grad.HasGrad) (VarT a_0)] -- ctx

-- type
(AppT
  (
    AppT (ConT Downhill.Grad.HasGrad) (ConT Main.MyRecord1)
  ) (VarT a_0)
)

[
  TySynInstD (
    TySynEqn Nothing (
      AppT (ConT Downhill.Grad.Scalar) (AppT (ConT Main.MyRecord1) (VarT a_0))
    ) (AppT (ConT Downhill.Grad.Scalar) (VarT a_0))
  )
]
-}

mkDVarC :: DVarOptions -> Q [Dec] -> Q [Dec]
mkDVarC options decs = concat <$> (traverse (mkDVarC1 options) =<< decs)
