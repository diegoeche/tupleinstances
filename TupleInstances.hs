{-# LANGUAGE TemplateHaskell, 
             MultiParamTypeClasses, 
             FlexibleInstances,
             FunctionalDependencies #-}
module Data.Tuple.Instances where

import THMacros

$(return $ map tupleNewType [2..15])

$(return $ map unTn [2..15])

$(return $ map functorTn [2..15])

class T a b | a -> b,  b -> a where
      unT   :: a -> b
      t     :: b -> a

instance T (T3 a) (a, a, a) where
         t = T3
         unT = unT3

-- example :: (Num a) => (a, a, a)
example1 = (unT . fmap (+1) $ T3 (1,2,3)) 

-- This one needs a explicit signature
example2 :: (Num a) => (a, a, a)
example2 = (unT . fmap (+1) $ t (1,2,3)) 