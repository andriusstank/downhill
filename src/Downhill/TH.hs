{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Downhill.TH where

import Control.Monad
import Downhill.Grad (HasGrad (Tang))
import Downhill.Linear.Expr (BasicVector (VecBuilder))
import Language.Haskell.TH
import Language.Haskell.TH.Syntax

curryN :: Int -> Q Exp
curryN n = do
  f <- newName "f"
  xs <- replicateM n (newName "x")
  let args = map VarP (f : xs)
      ntup = TupE (map VarE xs)
  return $ LamE args (AppE (VarE f) ntup)

someSplice :: Q [Dec]
someSplice = [d|y = 31|]

mkRecTangType :: VarBangType -> VarBangType
mkRecTangType (name, _bang, type_) =
  ( mkNameS (nameBase name),
    Bang NoSourceUnpackedness NoSourceStrictness,
    AppT (ConT ''Tang) type_
  )

mkTangType :: BangType -> BangType
mkTangType (_bang, type_) =
  ( Bang NoSourceUnpackedness NoSourceStrictness,
    AppT (ConT ''Tang) type_
  )

mkConstructorTang :: Con -> Q Con
mkConstructorTang c = do
  case c of
    NormalC name types -> do
      newConstrName <- newName (nameBase name ++ "Tang")
      return (NormalC newConstrName (map mkTangType types))
    RecC name types -> do
      newConstrName <- newName (nameBase name ++ "Tang")
      return (RecC newConstrName (map mkRecTangType types))
    _ -> fail ("Unknown constructor " ++ show c)

mkSemigroupInstance_ :: Name -> Name -> p -> Q [Dec]
mkSemigroupInstance_ name constrName types = do
  x1_0 <- newName "x"
  x2_2 <- newName "x"
  y1_1 <- newName "y"
  y2_3 <- newName "y"
  return $
    [InstanceD
      Nothing
      []
      (AppT (ConT ''Semigroup) (ConT name))
      [ FunD
          '(<>)
          [ Clause
              [ ConP constrName [VarP x1_0, VarP y1_1],
                ConP constrName [VarP x2_2, VarP y2_3]
              ]
              ( NormalB
                  ( AppE
                      ( AppE
                          (ConE constrName)
                          (InfixE (Just (VarE x1_0)) (VarE '(<>)) (Just (VarE x2_2)))
                      )
                      (InfixE (Just (VarE y1_1)) (VarE '(<>)) (Just (VarE y2_3)))
                  )
              )
              []
          ]
      ]]

mkRecordSemigroupInstance :: Name -> Q [Dec]
mkRecordSemigroupInstance recordName = do
  TyConI recordT <- reify recordName
  case recordT of
    DataD _cxt_ name tyvars kind constructors deriv ->
      case constructors of
        [contstr] ->
          case contstr of
            RecC constrName types ->
              mkSemigroupInstance_ recordName constrName types

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
  record <- reify recordName
  case record of
    TyConI recordT ->
      case recordT of
        DataD _cxt_ name tyvars kind constructors deriv ->
          case constructors of
            [] -> fail ("mkTang: " <> show recordName <> " has no data constructors")
            [contstr] -> do
              newConstr <- mkConstructorTang contstr
              newRecordName <- newName (nameBase name ++ "Tang")
              let tangDataType = DataD [] newRecordName tyvars kind [newConstr] deriv
              tangbasicvectorInstance <- mkRecordTangBasicVectorInstance newRecordName
              return $ [tangDataType] ++ tangbasicvectorInstance
            _ -> fail ("mkTang: " <> show recordName <> " has multiple data constructors")
        NewtypeD _cxt _name _tyvars _kind _con _deriv ->
          fail "mkTang: newtype not implemented"
        _ -> fail ("mkTang: " <> show recordName <> " is not a data type")
    _ -> fail ("mkTang: " <> show recordName <> " is not a type")

--x :: Q Exp
--x = runQ [e|1 + 2|]
