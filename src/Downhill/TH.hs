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
  ( mkDVar,
    RecordNamer,
    DVarOptions,
    defaultDVarOptions,
  )
where

import Control.Monad
import Data.AdditiveGroup ((^+^))
import Data.Functor.Identity (Identity (Identity, runIdentity))
import Data.VectorSpace (AdditiveGroup (zeroV, negateV))
import Downhill.Grad (HasGrad (Grad, Tang))
import Downhill.Linear.Expr (BasicVector (VecBuilder, sumBuilder))
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Data.AdditiveGroup ((^-^))

data DatatypeFields f
  = NormalFields [f Type]
  | RecordFields [f (String, Type)]

data DownhillDataType f = DownhillDataType
  { ddtTypeConName :: f Name,
    ddtDataConName :: f Name,
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

parseGradConstructor :: Name -> Con -> Q (DownhillDataType Identity)
parseGradConstructor tyName c = case c of
  NormalC name types ->
    return $
      DownhillDataType
        { ddtTypeConName = Identity tyName,
          ddtDataConName = Identity name,
          ddtFieldCount = length types,
          ddtFields = NormalFields (map (Identity . snd) types)
        }
  RecC name types ->
    return $
      DownhillDataType
        { ddtTypeConName = Identity tyName,
          ddtDataConName = Identity name,
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

mkSemigroupInstance :: DownhillDataType Identity -> Q [Dec]
mkSemigroupInstance record = do
  let n = ddtFieldCount record
  xs <- replicateM n (newName "x")
  ys <- replicateM n (newName "y")
  let xys = zipWith go xs ys
      go x y = InfixE (Just (VarE x)) (VarE '(<>)) (Just (VarE y))
  let recordType = return (ConT (runIdentity (ddtTypeConName record)))
      leftPat = return (ConP (runIdentity (ddtDataConName record)) (map VarP xs))
      rightPat = return (ConP (runIdentity (ddtDataConName record)) (map VarP ys))
      rhs = return (foldl AppE (ConE (runIdentity (ddtDataConName record))) xys)
  [d|
    instance Semigroup $recordType where
      $leftPat <> $rightPat = $rhs
    |]

mkAdditiveGroupInstance :: DownhillDataType Identity -> Q [Dec]
mkAdditiveGroupInstance record = do
  let n = ddtFieldCount record
  xs <- replicateM n (newName "x")
  ys <- replicateM n (newName "y")
  let 
      zipExp :: Name -> Name -> Name -> Exp
      zipExp f x y = InfixE (Just (VarE x)) (VarE f) (Just (VarE y))
      recordType = return (ConT (runIdentity (ddtTypeConName record)))
      leftPat = return (ConP (runIdentity (ddtDataConName record)) (map VarP xs))
      rightPat = return (ConP (runIdentity (ddtDataConName record)) (map VarP ys))
      zipRecord :: Name -> Q Exp
      zipRecord f = construct (zipWith (zipExp f) xs ys)
      rhsNegateV :: Q Exp
      rhsNegateV = construct (mapFields 'negateV xs)
      mapFields :: Name -> [Name] -> [Exp]
      mapFields f xs' = AppE (VarE f) . VarE <$> xs'
      rhsZeroV :: Q Exp
      rhsZeroV = construct (replicate n (VarE 'zeroV))
      construct :: [Exp] -> Q Exp
      construct x = return (foldl AppE (ConE (runIdentity (ddtDataConName record))) x)
  [d|
    instance AdditiveGroup $recordType where
      zeroV = $rhsZeroV
      negateV $leftPat = $rhsNegateV
      $leftPat ^+^ $rightPat = $(zipRecord '(^+^))
      $leftPat ^-^ $rightPat = $(zipRecord '(^-^))
    |]

mkBasicVectorInstance :: DownhillDataType DownhillVectorType -> Q [Dec]
mkBasicVectorInstance record = do
  let n = ddtFieldCount record
      vectorType = return (ConT (dvtVector (ddtTypeConName record)))
      builderType = return (ConT (dvtBuilder (ddtTypeConName record)))
  builders <- replicateM n (newName "x")
  let typeConName = dvtBuilder (ddtDataConName record)
      dataConName = dvtVector (ddtDataConName record)
  let pat :: Q Pat
      pat = return (ConP typeConName (map VarP builders))
      rhs :: Q Exp
      rhs =
        return
          ( foldl
              AppE
              (ConE dataConName)
              [AppE (VarE 'sumBuilder) (VarE x) | x <- builders]
          )
  [d|
    instance BasicVector $vectorType where
      type VecBuilder $vectorType = Maybe $builderType
      sumBuilder Nothing = zeroV
      sumBuilder (Just $pat) = $rhs
    |]

mkRecord :: DownhillDataType Identity -> Q [Dec]
mkRecord record = do
  let newConstr = mkConstructor record
  let newRecordName = runIdentity (ddtTypeConName record)
  let dataType = DataD [] newRecordName [] Nothing [newConstr] []
  return [dataType]

renameTypeS :: (String -> String) -> Name -> Name
renameTypeS f = mkNameS . f . nameBase

renameDownhillDataType' :: DVarOptions -> DownhillDataType Identity -> DownhillDataType DownhillTypes
renameDownhillDataType' options record =
  DownhillDataType
    { ddtTypeConName = renameCon typeConNamer (ddtTypeConName record),
      ddtDataConName = renameCon dataConNamer (ddtDataConName record),
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

mkDVar :: DVarOptions -> Name -> Q [Dec]
mkDVar options recordName = do
  record <- parseDownhillDataType recordName

  let downhillTypes :: DownhillDataType DownhillTypes
      downhillTypes = renameDownhillDataType' options record
      tangVector = mapDdt dtTang downhillTypes
      gradVector = mapDdt dtGrad downhillTypes

  tangDec <- mkRecord (mapDdt (Identity . dvtVector) tangVector)
  gradDec <- mkRecord (mapDdt (Identity . dvtVector) gradVector)
  tangBuilderDec <- mkRecord (mapDdt (Identity . dvtBuilder) tangVector)
  gradBuilderDec <- mkRecord (mapDdt (Identity . dvtBuilder) gradVector)
  tangSemigroup <- mkSemigroupInstance (mapDdt (Identity . dvtBuilder) tangVector)
  gradSemigroup <- mkSemigroupInstance (mapDdt (Identity . dvtBuilder) gradVector)
  tangInst <- mkBasicVectorInstance (mapDdt dtTang downhillTypes)
  gradInst <- mkBasicVectorInstance (mapDdt dtGrad downhillTypes)
  additiveTang <- mkAdditiveGroupInstance (mapDdt (Identity . dvtVector) tangVector)

  let decs =
        [ tangDec,
          gradDec,
          tangBuilderDec,
          gradBuilderDec,
          tangSemigroup,
          gradSemigroup,
          tangInst,
          gradInst,
          additiveTang
        ]
  return (concat decs)
