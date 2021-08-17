{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Downhill.TH where

import Control.Monad
import Data.VectorSpace (AdditiveGroup (zeroV))
import Downhill.Grad (HasGrad (Tang))
import Downhill.Linear.Expr (BasicVector (VecBuilder, sumBuilder))
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import System.Posix.Internals (c_dup2)
import Data.Maybe (isNothing)

data GradConstructors = GradConstructors
  { typeName :: Name,
    conName :: Name
  }

data GradDataType name
  = GradNormalDataType GradConstructors [Type]
  | GradRecordDataType GradConstructors [(String, Type)]

gdTypeName :: GradDataType Name -> String
gdTypeName = \case
  GradNormalDataType c _ -> nameBase (typeName c)
  GradRecordDataType c _ -> nameBase (typeName c)

data RecordNamer = RecordNamer
  { tyConNamer :: String -> String,
    dataConNamer :: String -> String,
    fieldNamer :: String -> String
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

{-
parseDataConstructor :: Con -> Q GradDataType
parseDataConstructor = \case
  NormalC name types -> return (GradNormalDataType (nameBase name) types)
-}

mkConstructor_ :: RecordNamer -> GradDataType Name -> Q Con
mkConstructor_ RecordNamer {dataConNamer} c = do
  case c of
    GradNormalDataType (GradConstructors _tyName dataName) types -> do
      newConstrName <- newName (dataConNamer (nameBase dataName))
      return (NormalC newConstrName (map mkTangType types))
    GradRecordDataType (GradConstructors _tyName dataName) types -> do
      newConstrName <- newName (dataConNamer (nameBase dataName))
      return (RecC newConstrName (map mkRecTangType types))
  where
    mkRecTangType :: (String, Type) -> VarBangType
    mkRecTangType (name, type_) =
      ( mkNameS name,
        Bang NoSourceUnpackedness NoSourceStrictness,
        AppT (ConT ''Tang) type_
      )

    mkTangType :: Type -> BangType
    mkTangType type_ =
      ( Bang NoSourceUnpackedness NoSourceStrictness,
        AppT (ConT ''Tang) type_
      )

parseGradConstructor :: Name -> Con -> Q (GradDataType Name)
parseGradConstructor tyName c = case c of
  NormalC name types ->
    let constrs = GradConstructors tyName name
        ctypes = map snd types
     in return $ GradNormalDataType constrs ctypes
  RecC name types ->
    let constrs = GradConstructors tyName name
        ctypes = map (\(fname, _, ty) -> (nameBase fname, ty)) types
     in return (GradRecordDataType constrs ctypes)
  _ -> fail ("Unsupported constructor type: " ++ show c)

parseGradDataType :: Name -> Q (GradDataType Name)
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

mkConstructorTang :: GradDataType Name -> Q Con
mkConstructorTang = mkConstructor_ defaultTangRecordNamer

mkConstructorGrad :: GradDataType Name -> Q Con
mkConstructorGrad = mkConstructor_ defaultGradRecordNamer

data GradProductType = GradProductType GradConstructors Int

mkGradProductType :: GradDataType Name -> GradProductType
mkGradProductType = \case
  GradNormalDataType cs fields -> GradProductType cs (length fields)
  GradRecordDataType cs fields -> GradProductType cs (length fields)

mkSemigroupInstance_ :: GradProductType -> Q [Dec]
mkSemigroupInstance_ (GradProductType (GradConstructors name constrName) n) = do
  xs <- replicateM n (newName "x")
  ys <- replicateM n (newName "y")
  let xys = zipWith go xs ys
      go x y = InfixE (Just (VarE x)) (VarE '(<>)) (Just (VarE y))
  let recordType = return (ConT name)
      leftPat = return (ConP constrName (map VarP xs))
      rightPat = return (ConP constrName (map VarP ys))
      rhs = return (foldl AppE (ConE constrName) xys)
  [d|
    instance Semigroup $recordType where
      $leftPat <> $rightPat = $rhs
    |]

mkRecordSemigroupInstance :: Name -> Q [Dec]
mkRecordSemigroupInstance recordName = do
  record <- parseGradDataType recordName
  mkSemigroupInstance_ (mkGradProductType record)

mkBasicVectorInstance_ :: Q Type -> Q Type -> DecsQ
mkBasicVectorInstance_ vectorType builderType =
  [d|
    instance BasicVector $vectorType where
      type VecBuilder $vectorType = Maybe $builderType
      sumBuilder Nothing = zeroV
      sumBuilder (Just _) = _
    |]

mkBasicVectorInstance vectorName builderName =
  mkBasicVectorInstance_ (return $ ConT vectorName) (return $ ConT builderName)

mkRecordTangBasicVectorInstance :: Name -> Q [Dec]
mkRecordTangBasicVectorInstance recordName = do
  let ihead :: Type
      ihead = AppT (ConT ''BasicVector) (ConT recordName)
      ctx :: Type
      ctx = AppT (ConT ''Monoid) (AppT (ConT ''VecBuilder) (ConT recordName))
  --ihead' <- [t|BasicVector recordName|]
  return [InstanceD Nothing [] ihead []]

mkTang :: Name -> Q [Dec]
mkTang recordName = do
  record <- parseGradDataType recordName
  newConstr <- mkConstructorTang record
  let newRecordName = gdTypeName record ++ "Tang"
  let tangDataType = DataD [] (mkNameS newRecordName) [] Nothing [newConstr] []
  --tangbasicvectorInstance <- mkRecordTangBasicVectorInstance newRecordName
  --return $ [tangDataType] ++ tangbasicvectorInstance
  return [tangDataType]

--x :: Q Exp
--x = runQ [e|1 + 2|]
