{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Downhill.TH where

import Control.Monad
import Data.VectorSpace (AdditiveGroup (zeroV))
import Downhill.Grad (HasGrad (Grad, Tang))
import Downhill.Linear.Expr (BasicVector (VecBuilder, sumBuilder))
import Language.Haskell.TH
import Language.Haskell.TH.Syntax

data DatatypeFields
  = NormalFields [Type]
  | RecordFields [(String, Type)]

data DownhillDataType = DownhillDataType
  { ddtTypeConName :: Name,
    ddtDataConName :: Name,
    ddtFieldCount :: Int,
    ddtFields :: DatatypeFields
  }

data DownhillVectorType = DownhillVectorType
  { dvtVector :: DownhillDataType,
    dvtBuilder :: DownhillDataType
  }

data RecordNamer = RecordNamer
  { typeConNamer :: String -> String,
    dataConNamer :: String -> String,
    fieldNamer :: String -> String
  }

data VectorNamer = VectorNamer
  { vnVector :: RecordNamer,
    vnBuilder :: RecordNamer
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

mkConstructor :: RecordNamer -> Name -> DownhillDataType -> Con
mkConstructor RecordNamer {dataConNamer, fieldNamer} tyfun record = do
  case ddtFields record of
    NormalFields types -> do
      let newConstrName = mkNameS (dataConNamer (nameBase (ddtDataConName record)))
      NormalC newConstrName (map mkType types)
    RecordFields types -> do
      let newConstrName = mkNameS (dataConNamer (nameBase (ddtDataConName record)))
      RecC newConstrName (map mkRecType types)
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

mkConstructor' :: Name -> DownhillDataType -> Con
mkConstructor' tyfun record =
  case ddtFields record of
    NormalFields types ->
      NormalC newConstrName (map mkType types)
    RecordFields types ->
      RecC newConstrName (map mkRecType types)
  where
    newConstrName :: Name
    newConstrName = ddtDataConName record
    mkRecType :: (String, Type) -> VarBangType
    mkRecType (name, type_) =
      ( mkNameS name,
        Bang NoSourceUnpackedness NoSourceStrictness,
        AppT (ConT tyfun) type_
      )

    mkType :: Type -> BangType
    mkType type_ =
      ( Bang NoSourceUnpackedness NoSourceStrictness,
        AppT (ConT tyfun) type_
      )

parseGradConstructor :: Name -> Con -> Q DownhillDataType
parseGradConstructor tyName c = case c of
  NormalC name types ->
    return $
      DownhillDataType
        { ddtTypeConName = tyName,
          ddtDataConName = name,
          ddtFieldCount = length types,
          ddtFields = NormalFields (map snd types)
        }
  RecC name types ->
    return $
      DownhillDataType
        { ddtTypeConName = tyName,
          ddtDataConName = name,
          ddtFieldCount = length types,
          ddtFields = RecordFields [(nameBase fname, ty) | (fname, _, ty) <- types]
        }
  _ -> fail ("Unsupported constructor type: " ++ show c)

parseDownhillDataType :: Name -> Q DownhillDataType
parseDownhillDataType recordName = do
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

mkSemigroupInstance :: DownhillDataType -> Q [Dec]
mkSemigroupInstance record = do
  let n = ddtFieldCount record
  xs <- replicateM n (newName "x")
  ys <- replicateM n (newName "y")
  let xys = zipWith go xs ys
      go x y = InfixE (Just (VarE x)) (VarE '(<>)) (Just (VarE y))
  let recordType = return (ConT (ddtTypeConName record))
      leftPat = return (ConP (ddtDataConName record) (map VarP xs))
      rightPat = return (ConP (ddtDataConName record) (map VarP ys))
      rhs = return (foldl AppE (ConE (ddtDataConName record)) xys)
  [d|
    instance Semigroup $recordType where
      $leftPat <> $rightPat = $rhs
    |]

mkBasicVectorInstance :: DownhillVectorType -> DecsQ
mkBasicVectorInstance (DownhillVectorType vectorRecord builderRecord) = do
  let n = ddtFieldCount vectorRecord
      vectorType = return (ConT (ddtTypeConName vectorRecord))
      builderType = return (ConT (ddtTypeConName builderRecord))
  builders <- replicateM n (newName "x")
  let constrName = ddtDataConName builderRecord
  let pat = return (ConP constrName (map VarP builders))
      rhs =
        return
          ( foldl
              AppE
              (ConE (ddtDataConName vectorRecord))
              [AppE (VarE 'sumBuilder) (VarE x) | x <- builders]
          )
  [d|
    instance BasicVector $vectorType where
      type VecBuilder $vectorType = Maybe $builderType
      sumBuilder Nothing = zeroV
      sumBuilder (Just $pat) = $rhs
    |]

mkRecord :: Name -> DownhillDataType -> Q [Dec]
mkRecord tyfun record = do
  let newConstr = mkConstructor' tyfun record
  let newRecordName = ddtTypeConName record
  let dataType = DataD [] newRecordName [] Nothing [newConstr] []
  return [dataType]

composeNamers :: RecordNamer -> RecordNamer -> RecordNamer
composeNamers (RecordNamer f1 g1 h1) (RecordNamer f2 g2 h2) =
  RecordNamer (f1 . f2) (g1 . g2) (h1 . h2)

renameDownhillDataType :: RecordNamer -> DownhillDataType -> DownhillDataType
renameDownhillDataType namer record =
  DownhillDataType
    { ddtTypeConName = mkNameS (typeConNamer namer (nameBase (ddtTypeConName record))),
      ddtDataConName = mkNameS (dataConNamer namer (nameBase (ddtDataConName record))),
      ddtFieldCount = ddtFieldCount record,
      ddtFields = renameFields (ddtFields record)
    }
  where
    renameFields = \case
      NormalFields fs -> NormalFields fs
      RecordFields fs -> RecordFields [(fieldNamer namer name, ty) | (name, ty) <- fs]

renameVector :: DVarOptions -> RecordNamer -> DownhillDataType -> DownhillVectorType
renameVector options namer record = DownhillVectorType vectorRecord builderNames
  where
    vectorRecord = renameDownhillDataType namer record
    builderNames = renameDownhillDataType (optBuilerNamer options) vectorRecord

mkDVar :: DVarOptions -> Name -> Q [Dec]
mkDVar options recordName = do
  record <- parseDownhillDataType recordName

  let tangVector = renameVector options (optTangNamer options) record
      gradVector = renameVector options (optGradNamer options) record

  tangDec <- mkRecord ''Tang (dvtVector tangVector)
  tangBuilderDec <- mkRecord ''VecBuilder (dvtBuilder tangVector)
  gradDec <- mkRecord ''Grad (dvtVector gradVector)
  gradBuilderDec <- mkRecord ''VecBuilder (dvtBuilder gradVector)
  tangSemigroup <- mkSemigroupInstance (dvtBuilder tangVector)
  gradSemigroup <- mkSemigroupInstance (dvtBuilder gradVector)
  tangInst <- mkBasicVectorInstance tangVector
  gradInst <- mkBasicVectorInstance gradVector

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
