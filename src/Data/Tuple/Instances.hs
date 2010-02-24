{-# LANGUAGE TemplateHaskell, 
             MultiParamTypeClasses, 
             FlexibleInstances,
             FunctionalDependencies #-}

module Data.Tuple.Instances where

import Data.Tuple.THMacros
import Control.Applicative
import Data.Foldable

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
              appTnInstance,
              foldTnInstance]
         return $ f n)

