-- {-# LANGUAGE FlexibleInstances #-}
module Data.Tuple.THMacros where

import Language.Haskell.TH.Syntax
import Data.Function

-- Helpful class to create variables of the different syntax
-- elements
class Var a where
      var :: String -> a

instance Var Exp where
         var = VarE . mkName

instance Var Pat where
         var = VarP . mkName

instance Var Type where
         var = VarT . mkName

class Constructor a where
       con :: String -> a

instance Constructor Type where
       con = ConT . mkName

-- instance Constructor Exp where
--          con = ConE . mkName

-- instance Constructor ([Pat] -> Pat) where
--          con = ConP . mkName

-- Creates the type name Tn
tn :: Int -> Name
tn = mkName . ('T':) . show

tnConT :: Int -> Type
tnConT = ConT . tn

--- Creates a newtype of the form:
--- newtype Tn a = Tn (a,...a)
tupleNewType :: Int -> Dec
tupleNewType n = NewtypeD [] name [mkName "a"] tuple deriv
        where deriv = map mkName ["Show", "Eq", "Read", "Ord"]
              name  = tn n
              tuple = NormalC name [(NotStrict, naryTupleType n "a")]

--- Creates a tuple type of the form (a,...a)
naryTupleType :: Int -> String -> Type
naryTupleType n v | n <= 1      = undefined
                  | otherwise  = foldl AppT (TupleT n) (replicate n $ var v)

--- Functions named unTn that unbox the tuple inside of the Tn newtype
unTn :: Int -> Dec
unTn n = FunD unTname [def]
     where def = Clause [tn n `ConP` [var "x"]] (NormalB $ var "x") []
           unTname = mkName $ "unT" ++ show n

-- Helper to define a (<TypeClass> Tn).
tnTypeClass :: String -> Int -> Type
tnTypeClass s = (AppT $ con s) . tnConT


tuplePattern :: [String] -> Pat
tuplePattern vars = tn (length vars) `ConP` [TupP $ map var vars]

-- Defines the functor instance for the Tn type
functorTnInstance :: Int -> Dec
functorTnInstance n = InstanceD [] (tnTypeClass "Functor" n) [fmapN]
          where fmapN = mkName "fmap" `FunD` [def]
                def = Clause [var "f", tuplePattern vars] (NormalB body) []
                vars = ['a' : show x | x <- [1..n]]
                body = ConE (tn n) `AppE` (TupE $ map ((var "f" `AppE`) . var) vars)

-- Defines the T instance for the Tn type
tupleTnInstance :: Int -> Dec
tupleTnInstance n = InstanceD [] type' [t, unT]
                where type' = con "T" `AppT` ta `AppT` naryTupleType n "a"
                            where ta = tnConT n `AppT` var "a"
                      clause body = Clause [] (NormalB body) []
                      t   = mkName "t"   `FunD` [clause $ ConE (tn n)]
                      unT = mkName "unT" `FunD` [clause . var $ "unT" ++ show n]

-- Defines the applicative instance for the Tn type
appTnInstance :: Int -> Dec
appTnInstance n = InstanceD [] type' [pureN, star]
              where type' = tnTypeClass "Applicative" n
                    pureN = mkName "pure" `FunD` [Clause [var "a"] (NormalB pureNBody) []]
                    pureNBody  = ConE (tn n) `AppE` (TupE . replicate n $ var "a")
                    star  = mkName "<*>" `FunD` [Clause starPattern (NormalB starBody) []]
                    (f, a) = unzip [('f': show x, 'a': show x) | x <- [1..n]]
                    starPattern = [tuplePattern f, tuplePattern a]
                    starBody = ConE (tn n) `AppE` (TupE $ zipWith (AppE `on` var) f a)

foldTnInstance :: Int -> Dec
foldTnInstance n = InstanceD [] type' [foldr']
              where type' = tnTypeClass "Foldable" n
                    as = ['a': show x | x <- [1..n]]
                    foldr' = mkName "foldr" `FunD` [Clause pattern (NormalB foldrBody) []]
                    pattern = [var "f", var "b", tuplePattern as]
                    foldrBody  = foldr f (var "b") as
                               where f = (\ x y -> var "f" `AppE` x `AppE` y) . var
