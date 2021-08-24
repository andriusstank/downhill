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
import Data.VectorSpace (AdditiveGroup (negateV, zeroV), VectorSpace((*^)))
import qualified Data.VectorSpace as VectorSpace
import Downhill.Grad (Dual (evalGrad), HasGrad (Grad, Scalar, Tang))
import Downhill.Linear.Expr (BasicVector (VecBuilder, sumBuilder))
import Language.Haskell.TH
  ( Bang (Bang),
    Con (NormalC, RecC),
    Cxt,
    Dec (DataD, InstanceD),
    Exp (AppE, ConE, InfixE, VarE),
    Info (TyConI),
    Name,
    Pat (ConP, VarP, InfixP),
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

data DownhillVectorType a = DownhillVectorType
  { dvtVector :: a,
    dvtBuilder :: a
  }

data RecordNamer = RecordNamer
  { typeConNamer :: String -> String,
    dataConNamer :: String -> String,
    fieldNamer :: String -> String
  }

data DVarOptions = DVarOptions
  { optTangNamer :: RecordNamer,
    optGradNamer :: RecordNamer,
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

data DownhillTypes a = DownhillTypes
  { dtPoint :: a,
    dtTang :: DownhillVectorType a,
    dtGrad :: DownhillVectorType a
  }

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

mkSemigroupInstance :: Cxt -> DownhillDataType Identity -> [Type] -> Q [Dec]
mkSemigroupInstance mayCxt record instVars = do
  let n = ddtFieldCount record
  xs <- replicateM n (newName "x")
  ys <- replicateM n (newName "y")
  let xys = zipWith go xs ys
      go x y = InfixE (Just (VarE x)) (VarE '(<>)) (Just (VarE y))
  let recordType = ConT (runIdentity (ddtTypeConName record))
      leftPat = ConP (runIdentity (ddtDataConName record)) (map VarP xs)
      rightPat = ConP (runIdentity (ddtDataConName record)) (map VarP ys)
      rhs = foldl AppE (ConE (runIdentity (ddtDataConName record))) xys
      typevarName :: TyVarBndr -> Name
      typevarName (PlainTV name) = name
      typevarName (KindedTV name _) = name
      typevars :: [Type]
      typevars = VarT . typevarName <$> ddtTypeVars record
      ihead :: Type
      ihead = foldl AppT recordType instVars
  case mayCxt of
    cxt -> do
      let dec =
            FunD
              '(<>)
              [ Clause
                  [leftPat, rightPat]
                  (NormalB rhs)
                  []
              ]
          ihead' = AppT (ConT ''Semigroup) ihead
      return [InstanceD Nothing cxt ihead' [dec]]

mkAdditiveGroupInstance :: DownhillDataType Identity -> Cxt -> [Type] -> Q [Dec]
mkAdditiveGroupInstance record cxt instVars = do
  let n = ddtFieldCount record
  xs <- replicateM n (newName "x")
  ys <- replicateM n (newName "y")
  let zipExp :: Name -> Name -> Name -> Exp
      zipExp f x y = InfixE (Just (VarE x)) (VarE f) (Just (VarE y))
      recordType0 = ConT (runIdentity (ddtTypeConName record))
      recordType = foldl AppT recordType0 instVars
      leftPat = ConP (runIdentity (ddtDataConName record)) (map VarP xs)
      rightPat = ConP (runIdentity (ddtDataConName record)) (map VarP ys)
      zipRecord :: Name -> Exp
      zipRecord f = construct (zipWith (zipExp f) xs ys)
      rhsNegateV :: Exp
      rhsNegateV = construct (mapFields 'negateV xs)
      mapFields :: Name -> [Name] -> [Exp]
      mapFields f xs' = AppE (VarE f) . VarE <$> xs'
      rhsZeroV :: Exp
      rhsZeroV = construct (replicate n (VarE 'zeroV))
      construct :: [Exp] -> Exp
      construct x = foldl AppE (ConE (runIdentity (ddtDataConName record))) x
      ihead' = AppT (ConT ''AdditiveGroup) recordType
  let zeroVDec = ValD (VarP 'zeroV) (NormalB rhsZeroV) []
      negateDec =
        FunD
          'negateV
          [ Clause
              [leftPat]
              (NormalB rhsNegateV)
              []
          ]
      plusDec =
        FunD
          '(^+^)
          [ Clause
              [leftPat, rightPat]
              (NormalB (zipRecord '(^+^)))
              []
          ]
      minusDec =
        FunD
          '(^-^)
          [ Clause
              [leftPat, rightPat]
              (NormalB (zipRecord '(^-^)))
              []
          ]
  return
    [ InstanceD
        Nothing
        cxt
        ihead'
        [ zeroVDec,
          negateDec,
          plusDec,
          minusDec
        ]
    ]


mkVectorSpaceInstance :: DownhillDataType Identity -> Type -> Cxt -> [Type] -> Q [Dec]
mkVectorSpaceInstance record scalarType cxt instVars = do
  let n = ddtFieldCount record
  xs <- replicateM n (newName "x")
  lhsName <- newName "s"
  let rightPat = ConP (runIdentity (ddtDataConName record)) (map VarP xs)
      recordType0 = ConT (runIdentity (ddtTypeConName record))
      recordType = foldl AppT recordType0 instVars
      rhsMulVField :: Name -> Exp
      rhsMulVField y = InfixE (Just (VarE lhsName)) (VarE ('(*^))) (Just (VarE y))
      rhsMulV :: Exp
      rhsMulV = construct (map rhsMulVField xs)
      construct :: [Exp] -> Exp
      construct x = foldl AppE (ConE (runIdentity (ddtDataConName record))) x
      ihead' = AppT (ConT ''VectorSpace) recordType
  let 
      vmulDec =
        FunD
          '(*^)
          [ Clause
              [VarP lhsName, rightPat]
              (NormalB rhsMulV)
              []
          ]
      scalarTypeDec = TySynInstD
        ( TySynEqn
            Nothing
            (AppT (ConT ''VectorSpace.Scalar) recordType)
            scalarType
        )
  return
    [ InstanceD
        Nothing
        cxt
        ihead'
        [ scalarTypeDec
        , vmulDec
        ]
    ]

mkBasicVectorInstance :: DownhillDataType DownhillVectorType -> Cxt -> [Type] -> Q [Dec]
mkBasicVectorInstance record cxt instVars = do
  let n = ddtFieldCount record
      vectorType0 = ConT (dvtVector (ddtTypeConName record))
      vectorType = foldl AppT vectorType0 instVars
      builderType0 = ConT (dvtBuilder (ddtTypeConName record))
      builderType = foldl AppT builderType0 instVars
  builders <- replicateM n (newName "x")
  let typeConName = dvtBuilder (ddtDataConName record)
      dataConName = dvtVector (ddtDataConName record)
  let pat :: Pat
      pat = ConP typeConName (map VarP builders)
      rhs :: Exp
      rhs =
        foldl
          AppE
          (ConE dataConName)
          [AppE (VarE 'sumBuilder) (VarE x) | x <- builders]
      ihead' = AppT (ConT ''BasicVector) vectorType
  let vecbuilderDec =
        TySynInstD
          ( TySynEqn
              Nothing
              (AppT (ConT ''VecBuilder) vectorType)
              (AppT (ConT ''Maybe) builderType)
          )
      sumBuilderDec =
        FunD
          'sumBuilder
          [ Clause [ConP 'Nothing []] (NormalB (VarE 'zeroV)) [],
            Clause [ConP 'Just [pat]] (NormalB rhs) []
          ]
  return [InstanceD Nothing cxt ihead' [vecbuilderDec, sumBuilderDec]]

mkDualInstance :: DownhillDataType DownhillTypes -> Type -> Cxt -> [Type] -> Q [Dec]
mkDualInstance record scalarType cxt instVars = do
  scalarTypeName <- newName "s"
  let vecType = foldl AppT (ConT . dvtVector . dtTang $ ddtTypeConName record) instVars
      gradType = foldl AppT (ConT . dvtVector . dtGrad $ ddtTypeConName record) instVars
      ihead' = AppT (AppT (AppT (ConT ''Dual) (VarT scalarTypeName)) vecType) gradType
      scalarConstraintAdditive = AppT (ConT ''AdditiveGroup) (VarT scalarTypeName)
      scalarConstraintTang = AppT (AppT EqualityT (VarT scalarTypeName)) (AppT (ConT ''VectorSpace.Scalar) vecType)
      scalarConstraintGrad = AppT (AppT EqualityT (VarT scalarTypeName)) (AppT (ConT ''VectorSpace.Scalar) gradType)
      allConstraints =
        cxt
          ++ [scalarConstraintAdditive, scalarConstraintTang, scalarConstraintGrad]
  return [InstanceD Nothing allConstraints ihead' []]

{-  [d|
  instance BasicVector $vectorType where
    type VecBuilder $vectorType = Maybe $builderType
    sumBuilder Nothing = zeroV
    sumBuilder (Just $pat) = $rhs
  |]
  -}

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
          dtGrad = renameVector' (renameName optGradNamer x)
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
          dtGrad = mkVectorTypes (AppT (ConT ''Grad) t)
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
          dtGrad = mkVectorFields (fieldNamer (optGradNamer options) name, AppT (ConT ''Grad) t)
        }

mkDVar'' :: DVarOptions -> Cxt -> DownhillDataType DownhillTypes -> Type -> [Type] -> Q [Dec]
mkDVar'' options cxt downhillTypes scalarType instVars = do
  let tangVector = mapDdt dtTang downhillTypes
      gradVector = mapDdt dtGrad downhillTypes

  tangDec <- mkRecord (mapDdt (Identity . dvtVector) tangVector)
  gradDec <- mkRecord (mapDdt (Identity . dvtVector) gradVector)
  tangBuilderDec <- mkRecord (mapDdt (Identity . dvtBuilder) tangVector)
  gradBuilderDec <- mkRecord (mapDdt (Identity . dvtBuilder) gradVector)
  tangSemigroup <- mkSemigroupInstance cxt (mapDdt (Identity . dvtBuilder) tangVector) instVars
  gradSemigroup <- mkSemigroupInstance cxt (mapDdt (Identity . dvtBuilder) gradVector) instVars
  tangInst <- mkBasicVectorInstance (mapDdt dtTang downhillTypes) cxt instVars
  gradInst <- mkBasicVectorInstance (mapDdt dtGrad downhillTypes) cxt instVars
  additiveTang <- mkAdditiveGroupInstance (mapDdt (Identity . dvtVector) tangVector) cxt instVars
  additiveGrad <- mkAdditiveGroupInstance (mapDdt (Identity . dvtVector) gradVector) cxt instVars
  vspaceTang <- mkVectorSpaceInstance (mapDdt (Identity . dvtVector) tangVector) scalarType cxt instVars
  vspaceGrad <- mkVectorSpaceInstance (mapDdt (Identity . dvtVector) gradVector) scalarType cxt instVars
  dualInstance <- mkDualInstance downhillTypes scalarType cxt instVars

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
          dualInstance
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
  _ -> fail $ "Expected (T a1 ... an) in constraint"

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

        dvar <- mkDVar'' options cxt downhillTypes scalarType instVars

        let tangName = dvtVector . dtTang $ ddtTypeConName downhillTypes
            gradName = dvtVector . dtGrad $ ddtTypeConName downhillTypes
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

            hasgradInstance = InstanceD Nothing cxt type_ (decs ++ [tangTypeDec, gradTypeDec])
        return $ dvar ++ [hasgradInstance]
      --fail "not implemented A"
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
