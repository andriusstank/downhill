{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Downhill.TH where

import Control.Functor.Constrained (Category)
import Control.Monad
import Data.Maybe (isNothing)
import Data.Typeable (tyConName)
import Data.VectorSpace (AdditiveGroup (zeroV))
import Downhill.Grad (HasGrad (Grad, Tang))
import Downhill.Linear.Expr (BasicVector (VecBuilder, sumBuilder))
import Language.Haskell.TH
import Language.Haskell.TH.Syntax

data GradConstructors = GradConstructors
  { typeName :: Name,
    conName :: Name
  }

data DatatypeFields
  = NormalFields [Type]
  | RecordFields [(String, Type)]

data GradDataType = GradDataType
  { gdtTypeConName :: Name,
    gdtDataConName :: Name,
    gdtFieldCount :: Int,
    gdtFields :: DatatypeFields
  }

data RecordNamer = RecordNamer
  { tyConNamer :: String -> String,
    dataConNamer :: String -> String,
    fieldNamer :: String -> String
  }

data DVarOptions = DVarOptions
  { tangNamer :: RecordNamer,
    gradNamer :: RecordNamer,
    builderNamer :: RecordNamer
  }

defaultTangRecordNamer :: RecordNamer
defaultTangRecordNamer =
  RecordNamer
    { tyConNamer = (++ "Tang"),
      dataConNamer = (++ "Tang"),
      fieldNamer = id
    }

defaultGradRecordNamer :: RecordNamer
defaultGradRecordNamer =
  RecordNamer
    { tyConNamer = (++ "Grad"),
      dataConNamer = (++ "Grad"),
      fieldNamer = id
    }

defaultBuilderRecordNamer :: RecordNamer
defaultBuilderRecordNamer =
  RecordNamer
    { tyConNamer = (++ "Builder"),
      dataConNamer = (++ "Builder"),
      fieldNamer = id
    }

defaultDVarOptions :: DVarOptions
defaultDVarOptions =
  DVarOptions
    { tangNamer = defaultTangRecordNamer,
      gradNamer = defaultGradRecordNamer,
      builderNamer = defaultBuilderRecordNamer
    }

mkConstructor_ :: RecordNamer -> Name -> GradDataType -> Q Con
mkConstructor_ RecordNamer {dataConNamer, fieldNamer} tyfun record = do
  case gdtFields record of
    NormalFields types -> do
      let newConstrName = mkNameS (dataConNamer (nameBase (gdtDataConName record)))
      return (NormalC newConstrName (map mkType types))
    RecordFields types -> do
      let newConstrName = mkNameS (dataConNamer (nameBase (gdtDataConName record)))
      return (RecC newConstrName (map mkRecType types))
  where
    mkRecType :: (String, Type) -> VarBangType
    mkRecType (name, type_) =
      ( mkNameS (fieldNamer name),
        Bang NoSourceUnpackedness NoSourceStrictness,
        AppT (ConT tyfun) type_
      )

    mkType :: Type -> BangType
    mkType type_ =
      ( Bang NoSourceUnpackedness NoSourceStrictness,
        AppT (ConT tyfun) type_
      )

parseGradConstructor :: Name -> Con -> Q GradDataType
parseGradConstructor tyName c = case c of
  NormalC name types ->
    return $
      GradDataType
        { gdtTypeConName = tyName,
          gdtDataConName = name,
          gdtFieldCount = length types,
          gdtFields = NormalFields (map snd types)
        }
  RecC name types ->
    return $
      GradDataType
        { gdtTypeConName = tyName,
          gdtDataConName = name,
          gdtFieldCount = length types,
          gdtFields = RecordFields [(nameBase fname, ty) | (fname, _, ty) <- types]
        }
  _ -> fail ("Unsupported constructor type: " ++ show c)

parseGradDataType :: Name -> Q GradDataType
parseGradDataType recordName = do
  record <- reify recordName
  recordT <- case record of
    TyConI recordT' -> return recordT'
    _ -> fail (show recordName ++ " is not a type")

  (name, constructors) <- do
    case recordT of
      DataD _cxt name tyvars _kind constructors _deriv -> do
        unless (null tyvars) $
          fail (show recordName ++ " has type variables")
        return (name, constructors)
      _ -> fail (show recordName ++ " is not a data type")
  constr <- case constructors of
    [] -> fail (show recordName <> " has no data constructors")
    [constr'] -> return constr'
    _ -> fail (show recordName <> " has multiple data constructors")
  parseGradConstructor name constr

mkSemigroupInstance :: GradDataType -> Q [Dec]
mkSemigroupInstance record = do
  let n = gdtFieldCount record
  xs <- replicateM n (newName "x")
  ys <- replicateM n (newName "y")
  let xys = zipWith go xs ys
      go x y = InfixE (Just (VarE x)) (VarE '(<>)) (Just (VarE y))
  let recordType = return (ConT (gdtTypeConName record))
      leftPat = return (ConP (gdtDataConName record) (map VarP xs))
      rightPat = return (ConP (gdtDataConName record) (map VarP ys))
      rhs = return (foldl AppE (ConE (gdtDataConName record)) xys)
  [d|
    instance Semigroup $recordType where
      $leftPat <> $rightPat = $rhs
    |]

mkBasicVectorInstance_ :: Q Type -> Q Type -> GradDataType -> GradDataType -> DecsQ
mkBasicVectorInstance_ vectorType builderType vectorRecord builderRecord = do
  let n = gdtFieldCount vectorRecord
  xs <- replicateM n (newName "x")
  let constrName = gdtDataConName builderRecord
  let pat = return (ConP constrName (map VarP xs))
      rhs =
        return
          ( foldl
              AppE
              (ConE (gdtDataConName vectorRecord))
              (map (AppE (VarE 'sumBuilder) . VarE) xs)
          )
  [d|
    instance BasicVector $vectorType where
      type VecBuilder $vectorType = Maybe $builderType
      sumBuilder Nothing = zeroV
      sumBuilder (Just $pat) = $rhs
    |]

mkBasicVectorInstance :: Name -> Name -> GradDataType -> GradDataType -> DecsQ
mkBasicVectorInstance vectorName builderName =
  mkBasicVectorInstance_ (return $ ConT vectorName) (return $ ConT builderName)

mkRecord :: RecordNamer -> Name -> GradDataType -> Q (Name, [Dec])
mkRecord namer tyfun record = do
  newConstr <- mkConstructor_ namer tyfun record
  let newRecordName = mkNameS (dataConNamer namer (nameBase (gdtTypeConName record)))
  let dataType = DataD [] newRecordName [] Nothing [newConstr] []
  return (newRecordName, [dataType])

composeNamers :: RecordNamer -> RecordNamer -> RecordNamer
composeNamers (RecordNamer f1 g1 h1) (RecordNamer f2 g2 h2) =
  RecordNamer (f1 . f2) (g1 . g2) (h1 . h2)

renameRecord :: RecordNamer -> GradConstructors -> GradConstructors
renameRecord namer cs =
  GradConstructors
    { typeName = mkNameS (tyConNamer namer (nameBase (typeName cs))),
      conName = mkNameS (dataConNamer namer (nameBase (conName cs)))
    }

renameGradProductType :: RecordNamer -> GradDataType -> GradDataType
renameGradProductType namer record =
  GradDataType
    { gdtTypeConName = mkNameS (tyConNamer namer (nameBase (gdtTypeConName record))),
      gdtDataConName = mkNameS (dataConNamer namer (nameBase (gdtDataConName record))),
      gdtFieldCount = gdtFieldCount record,
      gdtFields = renameFields (gdtFields record)
    }
  where
    renameFields = \case
      NormalFields fs -> NormalFields fs
      RecordFields fs -> RecordFields [(fieldNamer namer name, ty) | (name, ty) <- fs]

mkDVar :: DVarOptions -> Name -> Q [Dec]
mkDVar options recordName = do
  record <- parseGradDataType recordName
  (tangName, tangDec) <- mkRecord (tangNamer options) ''Tang record
  (gradName, gradDec) <- mkRecord (gradNamer options) ''Grad record
  let tangVectorRecord = renameGradProductType (tangNamer options) record
      gradVectorRecord = renameGradProductType (gradNamer options) record

  let tangBuilderNamer = composeNamers (builderNamer options) (tangNamer options)
  let gradBuilderNamer = composeNamers (builderNamer options) (gradNamer options)
  (tangBuilderName, tangBuilderDec) <- mkRecord tangBuilderNamer ''VecBuilder record
  (gradBuilderName, gradBuilderDec) <- mkRecord gradBuilderNamer ''VecBuilder record
  let tangBuilderNames = renameGradProductType tangBuilderNamer record
      gradBuilderNames = renameGradProductType gradBuilderNamer record
  tangSemigroup <- mkSemigroupInstance tangBuilderNames
  gradSemigroup <- mkSemigroupInstance gradBuilderNames
  tangInst <- mkBasicVectorInstance tangName tangBuilderName tangVectorRecord tangBuilderNames
  gradInst <- mkBasicVectorInstance gradName gradBuilderName gradVectorRecord gradBuilderNames
  let decs =
        [ tangDec,
          gradDec,
          tangBuilderDec,
          gradBuilderDec,
          tangSemigroup,
          gradSemigroup,
          tangInst,
          gradInst
        ]
  return (concat decs)
