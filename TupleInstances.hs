{-# LANGUAGE TemplateHaskell #-}
module Data.Tuple.Instances where

import THMacros

$(return $ map tupleNewType [2..15])

$(return $ map unTn [2..15])

$(return $ map functorTn [2..15])






