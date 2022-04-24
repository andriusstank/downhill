{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Use like this:
--
-- @
-- mkHasGradInstances
--   defaultBVarOptions
--   [d|
--     instance HasGrad MyRecord where
--       type MScalar MyRecord = Float
--     |]
-- @
--
-- Instance declaration passed to @mkHasGradInstances@ gives two important bits of information:
--
--   * Type variables for @MyRecord@, which can be concrete types (such as @instance HasGrad (MyRecord Float)@)
--     or regular type variables (@instance HasGrad (MyRecord a)@)
--
--   * Scalar type.
module Downhill.TH
  ( mkHasGradInstances,
    AffineSpaceOptions (..),
    RecordNamer (..),
    TangOptions (..),
    GradOptions (..),
    BVarOptions (..),
    defaultBVarOptions,
  )
where

import Control.Monad
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Writer.CPS (WriterT, execWriterT, tell)
import Data.AdditiveGroup ((^+^), (^-^))
import Data.AffineSpace (AffineSpace (Diff, (.+^), (.-.)))
import Data.Foldable (traverse_)
import qualified Data.Map as Map
import Data.Maybe (catMaybes)
import Data.VectorSpace (AdditiveGroup (negateV, zeroV), VectorSpace (Scalar, (*^)))
import Downhill.BVar (BVar (BVar))
import Downhill.Grad
  ( Dual (evalGrad),
    HasGrad (Grad, MScalar, Tang),
  )
import Downhill.Linear.Expr (BasicVector (VecBuilder, identityBuilder, sumBuilder))
import Downhill.Linear.Lift (lift1_sparse)
import GHC.Records (HasField (getField))
import Language.Haskell.TH
  ( Bang (Bang),
    Con (NormalC, RecC),
    Cxt,
    Dec (DataD, InstanceD, NewtypeD, SigD),
    Exp (AppE, ConE, InfixE, VarE),
    Name,
    Pat (VarP),
    Q,
    SourceStrictness (NoSourceStrictness),
    SourceUnpackedness (NoSourceUnpackedness),
    Type (AppT, ConT, VarT),
    nameBase,
    newName,
  )
import qualified Language.Haskell.TH
import Language.Haskell.TH.Datatype (ConstructorInfo (constructorFields, constructorName, constructorVariant), ConstructorVariant (InfixConstructor, NormalConstructor, RecordConstructor), DatatypeInfo (datatypeCons, datatypeInstTypes, datatypeName, datatypeVariant, datatypeVars), DatatypeVariant (Newtype), TypeSubstitution (applySubstitution), reifyDatatype)
import Language.Haskell.TH.Datatype.TyVarBndr (TyVarBndrUnit)
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

type THWriter = WriterT [Dec] Q

data DatatypeFields
  = NormalFields [Type]
  | RecordFields [(String, Type)]
  deriving (Show)

data DownhillRecord = DownhillRecord
  { ddtTypeConName :: Name,
    ddtDataConName :: Name,
    ddtFieldTypes :: [Type],
    ddtFieldNames :: Maybe [String],
    ddtTypeVars :: [TyVarBndrUnit],
    ddtFieldCount :: Int,
    ddtVariant :: DatatypeVariant
  }
  deriving (Show)

-- | Sometimes new data type needs to be generated as a counterpart for the given one.
-- @RecordNamer@ will generated new names for them, given the names of original data type.
data RecordNamer = RecordNamer
  { typeConNamer :: String -> String,
    dataConNamer :: String -> String,
    fieldNamer :: String -> String
  }

data RecordTranstorm = RecordTranstorm RecordNamer (Type -> Type)

data AffineSpaceOptions
  = -- | Generate AffineSpace instance
    MakeAffineSpace
  | -- | Don't generate AffineSpace instance
    NoAffineSpace
  | -- | Generate AffineSpace instance if @optExcludeFields@ is empty
    AutoAffineSpace

data BVarOptions = BVarOptions
  { optTang :: TangOptions,
    optGrad :: GradOptions,
    optBuilderNamer :: RecordNamer,
    optAffineSpace :: AffineSpaceOptions,
    optHasFieldInstances :: Bool,
    -- | List of fields that take no part in differentiation
    optExcludeFields :: [String]
  }

pattern ConP :: Name -> [Pat] -> Pat
#if MIN_VERSION_template_haskell(2,18,0)
pattern ConP x y = Language.Haskell.TH.ConP x [] y
#else
pattern ConP x y = Language.Haskell.TH.ConP x y
#endif

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

defaultBuilderRecordNamer :: RecordNamer
defaultBuilderRecordNamer =
  RecordNamer
    { typeConNamer = (++ "Builder"),
      dataConNamer = (++ "Builder"),
      fieldNamer = id
    }

-- | @SameTang@ will use given type as its own tangent space. That's the right options
-- for vector spaces.
-- @GenTang namer@ will generate new data type the tangent space. Affine spaces, manifolds
-- will need this option.
data TangOptions = SameTang | GenTang RecordNamer

-- | @SameGrad@ won't generate separate data type for gradient.
-- Useful for Hilbert spaces
-- when we don't want to distinguish between values of the variables and their gradients.
-- @GenGrad namer@ will generate distinct data type for gradients.
data GradOptions = SameGrad | GenGrad RecordNamer

defaultBVarOptions :: BVarOptions
defaultBVarOptions =
  BVarOptions
    { optTang = GenTang defaultTangRecordNamer,
      optGrad = GenGrad defaultGradRecordNamer,
      optBuilderNamer = defaultBuilderRecordNamer,
      optAffineSpace = AutoAffineSpace,
      optHasFieldInstances = True,
      optExcludeFields = []
    }

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

parseGradConstructor :: Monad m => Name -> DatatypeInfo -> ConstructorInfo -> [TyVarBndrUnit] -> m DownhillRecord
parseGradConstructor tyName dinfo cinfo typevars = do
  let types = constructorFields cinfo
      n = length types
  (fieldTypes, fieldNames) <- case constructorVariant cinfo of
    NormalConstructor -> return (types, Nothing)
    InfixConstructor -> return (types, Nothing)
    RecordConstructor fieldNames -> do
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

parseDownhillRecord :: (Monad m, MonadFail m) => Name -> DatatypeInfo -> m (DownhillRecord, ConstructorInfo)
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
elementwiseOp record = elementwiseOp' record record record

elementwiseOp' :: DownhillRecord -> DownhillRecord -> DownhillRecord -> Name -> Q Dec
elementwiseOp' leftRecord rightRecord resRecord func = do
  let n = ddtFieldCount resRecord
  --dataConName :: Name
  --dataConName = ddtDataConName record
  xs <- replicateM n (newName "x")
  ys <- replicateM n (newName "y")
  let fieldOp :: Name -> Name -> Exp
      fieldOp x y = InfixE (Just (VarE x)) (VarE func) (Just (VarE y))
      resultFields :: [Exp]
      resultFields = zipWith fieldOp xs ys
      leftPat = ConP (ddtDataConName leftRecord) (map VarP xs)
      rightPat = ConP (ddtDataConName rightRecord) (map VarP ys)
      rhs :: Exp
      rhs = foldl AppE (ConE (ddtDataConName resRecord)) resultFields
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

mkClassInstance :: Name -> Cxt -> DownhillRecord -> [Type] -> [Dec] -> THWriter ()
mkClassInstance className cxt record instVars decs = do
  let recordType = ConT (ddtTypeConName record)
      ihead = AppT (ConT className) (foldl AppT recordType instVars)
  tell [InstanceD Nothing cxt ihead decs]

mkSemigroupInstance :: Cxt -> DownhillRecord -> [Type] -> THWriter ()
mkSemigroupInstance cxt record instVars = do
  dec <- lift (elementwiseOp record '(<>))
  mkClassInstance ''Semigroup cxt record instVars [dec]

mkAdditiveGroupInstance :: Cxt -> DownhillRecord -> [Type] -> THWriter ()
mkAdditiveGroupInstance cxt record instVars = do
  decs <- lift $ do
    zeroVDec <- elementwiseValue record 'zeroV
    negateDec <- elementwiseFunc record 'negateV
    plusDec <- elementwiseOp record '(^+^)
    minusDec <- elementwiseOp record '(^-^)
    return
      [ zeroVDec,
        negateDec,
        plusDec,
        minusDec
      ]
  mkClassInstance ''AdditiveGroup cxt record instVars decs

mkVectorSpaceInstance :: DownhillRecord -> Type -> Cxt -> [Type] -> THWriter ()
mkVectorSpaceInstance record scalarType cxt instVars = do
  let n = ddtFieldCount record
      dataConName :: Name
      dataConName = ddtDataConName record
  xs <- lift $ case ddtFieldNames record of
    Nothing -> replicateM n (newName "x")
    Just names -> traverse newName names

  lhsName <- lift (newName "s")
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

mkBasicVectorInstance :: DownhillRecord -> BVarOptions -> Cxt -> [Type] -> THWriter ()
mkBasicVectorInstance vectorRecord options cxt instVars = do
  sumBuilderDec <- lift mkSumBuilder
  identityBuilderDec <- lift mkIdentityBuilder
  mkClassInstance ''BasicVector cxt vectorRecord instVars [vecbuilderDec, sumBuilderDec, identityBuilderDec]
  where
    n = ddtFieldCount vectorRecord
    builderRecord = renameDownhillRecord (builderTransform options) vectorRecord

    -- not an elementiseOp, because right hand side is wrapped in Maybe
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

    mkIdentityBuilder :: Q Dec
    mkIdentityBuilder = do
      builders <- replicateM n (newName "x")
      let pat :: Pat
          pat = ConP (ddtDataConName vectorRecord) (map VarP builders)
          rhs :: Exp
          rhs =
            AppE
              (ConE 'Just)
              ( foldl
                  AppE
                  (ConE (ddtDataConName builderRecord))
                  [AppE (VarE 'identityBuilder) (VarE x) | x <- builders]
              )
      return $
        FunD
          'identityBuilder
          [ Clause [pat] (NormalB rhs) []
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
  THWriter ()
mkDualInstance tangRecord gradRecord scalarType cxt instVars = do
  when (ddtFieldCount tangRecord /= ddtFieldCount gradRecord) $
    fail "mkDualInstance: ddtFieldCount tangRecord /= ddtFieldCount gradRecord"
  scalarTypeName <- lift (newName "s")
  mkClassDec (VarT scalarTypeName)
  where
    n = ddtFieldCount tangRecord

    -- instance (cxt, AdditiveGroup s, s ~ scalarType) => AdditiveGroup (Record a1 … an) where
    --   …
    mkClassDec :: Type -> THWriter ()
    mkClassDec scalarVar = do
      evalGradDec <- lift mkEvalGradDec
      tell [InstanceD Nothing (cxt ++ newConstraints) ihead [evalGradDec]]
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

mkRecord :: DownhillRecord -> THWriter ()
mkRecord record = do
  let newConstr = mkConstructor record
  let newRecordName = ddtTypeConName record
  let dataType = case ddtVariant record of
        Newtype -> NewtypeD [] newRecordName (ddtTypeVars record) Nothing newConstr []
        _ -> DataD [] newRecordName (ddtTypeVars record) Nothing [newConstr] []
  tell [dataType]

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
  THWriter ()
mkGetField pointRecord gradBuilderRecord cxt instVars field = do
  rName <- lift $ newName "r"
  xName <- lift $ newName "x"
  dxName <- lift $ newName "dx"
  goName <- lift $ newName "go"
  dxdaName <- lift $ newName "dx_da"
  let rhsFieldList :: [Exp]
      rhsFieldList =
        replicate (fiIndex field) (VarE 'mempty)
          ++ [VarE dxdaName]
          ++ replicate (n - fiIndex field - 1) (VarE 'mempty)
      -- rhs = MyRecordGradBuilder mempty … mempty dx_da_a6SX mempty … mempty
      rhs :: Exp
      rhs = foldl AppE (ConE (ddtDataConName gradBuilderRecord)) rhsFieldList
  tell
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

renameDownhillRecord :: RecordTranstorm -> DownhillRecord -> DownhillRecord
renameDownhillRecord (RecordTranstorm namer typeFun) record =
  DownhillRecord
    { ddtTypeConName = renameTypeS (typeConNamer namer) (ddtTypeConName record),
      ddtDataConName = renameTypeS (dataConNamer namer) (ddtDataConName record),
      ddtTypeVars = ddtTypeVars record,
      ddtFieldCount = ddtFieldCount record,
      ddtFieldTypes = typeFun <$> ddtFieldTypes record,
      ddtFieldNames = fmap (fmap (fieldNamer namer)) (ddtFieldNames record),
      ddtVariant = ddtVariant record
    }

builderTransform :: BVarOptions -> RecordTranstorm
builderTransform options = RecordTranstorm (optBuilderNamer options) (AppT (ConT ''VecBuilder))

tangTransform :: BVarOptions -> Maybe RecordTranstorm
tangTransform options = case optTang options of
  SameTang -> Nothing
  GenTang tangNamer -> Just (RecordTranstorm tangNamer (AppT (ConT ''Tang)))

gradTransform :: BVarOptions -> Maybe RecordTranstorm
gradTransform options = case optGrad options of
  SameGrad -> Nothing
  GenGrad gradNamer -> Just (RecordTranstorm gradNamer (AppT (ConT ''Grad)))

mkVecInstances :: Cxt -> [Type] -> Type -> DownhillRecord -> BVarOptions -> THWriter ()
mkVecInstances cxt instVars scalarType vectorType options = do
  let builderType = renameDownhillRecord (builderTransform options) vectorType
  mkRecord builderType
  mkSemigroupInstance cxt builderType instVars
  mkBasicVectorInstance vectorType options cxt instVars
  mkAdditiveGroupInstance cxt vectorType instVars
  mkVectorSpaceInstance vectorType scalarType cxt instVars

mkVecTrVec :: Cxt -> [Type] -> Type -> DownhillRecord -> Maybe RecordTranstorm -> BVarOptions -> THWriter DownhillRecord
mkVecTrVec cxt instVars scalarType origRecordType transform options = do
  maybeRenamed <- case transform of
    Nothing -> return origRecordType
    Just tr -> do
      let renamed = renameDownhillRecord tr origRecordType
      mkRecord renamed
      return renamed
  mkVecInstances cxt instVars scalarType maybeRenamed options
  return maybeRenamed

data MkDVarResult = MkDVarResult
  { mkdvTangName :: Name,
    mkdvGradName :: Name
  }

mkDVar'' ::
  Cxt ->
  DownhillRecord ->
  BVarOptions ->
  Type ->
  [Type] ->
  ConstructorInfo ->
  THWriter MkDVarResult
mkDVar'' cxt pointRecord options scalarType instVars substitutedCInfo = do
  tangRecord <- mkVecTrVec cxt instVars scalarType pointRecord (tangTransform options) options
  gradRecord <- mkVecTrVec cxt instVars scalarType pointRecord (gradTransform options) options

  {-
  -- No VectorSpace for MetricTensor
  additiveMetric <- mkAdditiveGroupInstance cxt metricRecord instVars
  vspaceMetric <- mkVectorSpaceInstance metricRecord scalarType cxt instVars
  -}
  mkDualInstance tangRecord gradRecord scalarType cxt instVars
  let needAffineSpace = case optAffineSpace options of
        MakeAffineSpace -> True
        NoAffineSpace -> False
        AutoAffineSpace -> null (optExcludeFields options)

  when needAffineSpace $
    mkAffineSpaceInstance cxt pointRecord tangRecord instVars

  when (optHasFieldInstances options) $
    case ddtFieldNames pointRecord of
      Nothing -> return ()
      Just names ->
        let info :: Int -> String -> Type -> FieldInfo
            info index name = FieldInfo name index
            substitutedFields = constructorFields substitutedCInfo
            fields :: [FieldInfo]
            fields = zipWith3 info [0 ..] names substitutedFields
            mkFieldGetter :: FieldInfo -> THWriter ()
            mkFieldGetter =
              mkGetField
                pointRecord
                (renameDownhillRecord (builderTransform options) gradRecord)
                cxt
                instVars
        in traverse_ mkFieldGetter fields
  return
    MkDVarResult
      { mkdvTangName = ddtTypeConName tangRecord,
        mkdvGradName = ddtTypeConName gradRecord
      }

parseRecordType :: (Monad m, MonadFail m) => Type -> [Type] -> m (Name, [Type])
parseRecordType type_ vars = case type_ of
  AppT inner typeVar -> parseRecordType inner (typeVar : vars)
  ConT recordName -> return (recordName, vars)
  _ -> fail "Expected (T a1 ... an) in constraint"

mkAffineSpaceInstance :: Cxt -> DownhillRecord -> DownhillRecord -> [Type] -> THWriter ()
mkAffineSpaceInstance cxt recordPoint recordTang instVars = do
  plusDec <- lift (elementwiseOp' recordPoint recordTang recordPoint '(.+^))
  minusDec <- lift (elementwiseOp' recordPoint recordPoint recordTang '(.-.))
  let recordTypePoint = foldl AppT (ConT (ddtTypeConName recordPoint)) instVars
      recordTypeTang = foldl AppT (ConT (ddtTypeConName recordTang)) instVars
      diffTypeDec =
        TySynInstD
          ( TySynEqn
              Nothing
              (AppT (ConT ''Diff) recordTypePoint)
              recordTypeTang
          )
  let decs =
        [ plusDec,
          minusDec,
          diffTypeDec
        ]
  mkClassInstance ''AffineSpace cxt recordPoint instVars decs

filterFields :: forall m. MonadFail m => BVarOptions -> DownhillRecord -> m DownhillRecord
filterFields options record =
  case optExcludeFields options of
    [] -> return record
    _ -> do
      fieldList <- case ddtFieldNames record of
        Just fields -> return fields
        Nothing -> fail (nameBase (ddtTypeConName record) ++ " is not a records, can't exclude fields")
      doFilterFields fieldList
  where
    doFilterFields fieldList = do
      traverse_ check (optExcludeFields options)
      return
        record
          { ddtFieldTypes = go (ddtFieldTypes record),
            ddtFieldNames = go <$> ddtFieldNames record,
            ddtFieldCount = goN (ddtFieldCount record)
          }
      where
        check :: String -> m ()
        check name
          | name `elem` fieldList = return ()
          | otherwise = fail ("Field " ++ name ++ " is not a member of " ++ nameBase (ddtTypeConName record))
        excludeZipList :: [x -> Maybe x]
        excludeZipList = filterField <$> fieldList
          where
            filterField :: String -> x -> Maybe x
            filterField fieldName x
              | fieldName `elem` optExcludeFields options = Nothing
              | otherwise = Just x
        go :: [a] -> [a]
        go = catMaybes . zipWith ($) excludeZipList
        goN :: Int -> Int
        goN n = length . go $ replicate n ()

mkDVarC1 :: BVarOptions -> Dec -> THWriter ()
mkDVarC1 options = \case
  InstanceD mayOverlap cxt type_ decs -> do
    case mayOverlap of
      Just _ -> fail "Overlapping instances not implemented"
      _ -> return ()
    case type_ of
      AppT (ConT hasgradCtx) recordInConstraintType -> do
        when (hasgradCtx /= ''HasGrad) $
          fail $ "Constraint must be `HasGrad`, got " ++ show hasgradCtx
        (recordName, instVars) <- parseRecordType recordInConstraintType []
        record' <- lift (reifyDatatype recordName)

        (fullParsedRecord, cinfo) <- parseDownhillRecord recordName record'
        parsedRecord <- filterFields options fullParsedRecord
        recordTypeVarNames <- do
          let getName x = case x of
                SigT (VarT y) _ -> return y
                _ -> fail "Type variable is not VarT"
          traverse getName (datatypeInstTypes record')
        -- We have two sets of type variables: one in record definition (as in `data MyRecord a b c = ...`)
        -- and another one in instance head (`instance HasGrad (MyRecord a' b' c')). We need
        -- those from instance head for HasField instances.
        let substPairs = zip recordTypeVarNames instVars
            substitutedRecord = applySubstitution (Map.fromList substPairs) cinfo

        scalarType <- case decs of
          [] -> fail "`HasGrad` instance has no declarations"
          [dec1] -> case dec1 of
            TySynInstD (TySynEqn _ (AppT (ConT scalarName) _) scalarType) -> do
              when (scalarName /= ''MScalar) $
                fail ("Expected `Scalar` equation, got " ++ show scalarName)
              return scalarType
            _ -> fail "HasGrad instance must contain `MScalar ... = ...` declaration"
          _ -> fail "`HasGrad` has multiple declarations"

        MkDVarResult {mkdvTangName, mkdvGradName} <-
          mkDVar'' cxt parsedRecord options scalarType instVars substitutedRecord

        --metricTypeDec' :: [Dec]
        --metricTypeDec' <- lift ( [d|type Metric recordInConstraintType = ()|] :: Q [Dec]

        let --tangName = ddtTypeConName (maybeRenameDownhillRecord (tangTransform options) parsedRecord)
            --gradName = ddtTypeConName (maybeRenameDownhillRecord (gradTransform options) parsedRecord)
            --metricName = ddtTypeConName (renameDownhillRecord (metricTransform options) parsedRecord)
            tangTypeDec =
              TySynInstD
                ( TySynEqn
                    Nothing
                    (AppT (ConT ''Tang) recordInConstraintType)
                    (foldl AppT (ConT mkdvTangName) instVars)
                )
            gradTypeDec =
              TySynInstD
                ( TySynEqn
                    Nothing
                    (AppT (ConT ''Grad) recordInConstraintType)
                    (foldl AppT (ConT mkdvGradName) instVars)
                )

            hasgradInstance =
              InstanceD
                Nothing
                cxt
                type_
                ( decs
                    ++ [ tangTypeDec,
                         gradTypeDec
                       ]
                )
        tell [hasgradInstance]
      _ -> fail "Instance head is not a constraint"
  _ -> fail "Expected instance declaration"

mkHasGradInstances' :: BVarOptions -> [Dec] -> WriterT [Dec] Q ()
mkHasGradInstances' options = traverse_ (mkDVarC1 options)

-- | Generates @HasGrad@ instance, along with @Tang@ and @Grad@ types,
-- @VecBuilder@ types and all other instances needed for @HasGrad@.
mkHasGradInstances :: BVarOptions -> Q [Dec] -> Q [Dec]
mkHasGradInstances options decs = execWriterT (mkHasGradInstances' options =<< lift decs)
