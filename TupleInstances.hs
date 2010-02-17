{-# LANGUAGE TemplateHaskell, 
             MultiParamTypeClasses, 
             FlexibleInstances,
             FunctionalDependencies #-}
module Data.Tuple.Instances where

import THMacros
import Control.Applicative

class T a b | a -> b,  b -> a where
      unT   :: a -> b
      t     :: b -> a

-- All the declarations
$(return $ do
         n <- [2..15]
         f <- [tupleNewType,
              unTn,
              functorTnInstance,
              tupleTnInstance,
              appTnInstance]
         return $ f n)

instance (Show a) => Show (T3 a) where
         show (T3 x) = "T3 " ++ show x

-- example :: (Num a) => (a, a, a)
example1 = (unT . fmap (+1) $ T3 (1,2,3))


-- This one needs a explicit signature
example2 :: (Num a) => (a, a, a)
example2 = unT . fmap (+1) $ t (1,2,3)

example3 = unT . fmap (+1) $ T10 (1,2,3,4,5,6,7,8,9,10)

example4 :: (Num a) => (a, a, a, a, a, a, a, a, a, a)
example4 = (unT . fmap (+1) $ t (1,2,3,4,5,6,7,8,9,10))
