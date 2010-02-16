-- {-# LANGUAGE FlexibleInstances #-}
module THMacros where

import Language.Haskell.TH.Syntax

class Var a where
      var :: String -> a

instance Var Exp where
         var = VarE . mkName

instance Var Pat where
         var = VarP . mkName

instance Var Type where
         var = VarT . mkName

-- class Constructor a where
--       con :: String -> a

-- instance Constructor Exp where
--          con = ConE . mkName

-- instance Constructor ([Pat] -> Pat) where
--          con = ConP . mkName

-- Creates the type name Tn
tn :: Int -> Name
tn = mkName . ('T':) . show

--- Creates a newtype of the form:
--- newtype Tn a = Tn (a,...a)
tupleNewType :: Int -> Dec
tupleNewType n = NewtypeD [] name [mkName "a"] tuple []
        where name = tn n
              tuple = NormalC name [(NotStrict, naryTupleType n "a")]

--- Creates a tuple type of the form (a,...a)
naryTupleType :: Int -> String -> Type
naryTupleType n v | n <= 1 = undefined
                  | otherwise = foldl AppT (TupleT n) (replicate n $ var v)

--- Functions named unTn that unbox the tuple inside of the Tn newtype
unTn :: Int -> Dec
unTn n = FunD unTname [def]
     where def = Clause [tn n `ConP` [var "x"]] (NormalB $ var "x") []
           unTname = mkName $ "unT" ++ show n

functorTn :: Int -> Dec
functorTn n = InstanceD [] ((ConT $ mkName "Functor") `AppT` (ConT $ tn n)) [fmapN]
          where fmapN =  mkName "fmap" `FunD` [def]
                def = Clause [var "f", tuplePattern] (NormalB body) []
                vars = ['a' : show x | x <- [1..n]]
                body = ConE (tn n) `AppE` (TupE $ map ((var "f" `AppE`) . var) vars)
                tuplePattern = tn n `ConP` [TupP $ map var vars]
