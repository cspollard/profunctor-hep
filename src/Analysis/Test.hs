{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE RankNTypes #-}

module Analysis.Test where

import Prelude hiding (id, (.))
import Control.Category
import Data.Profunctor
import Analysis.MealyMoore
import Analysis.Fold


data TF a = TF a a deriving (Functor, Foldable, Traversable, Show)


choose :: Bool -> Traversal' (TF a) a
choose True = true
choose False = false


true :: Lens' (TF a) a
true f = dimap (\(TF a b) -> (a, b)) (\(a, b) -> TF a b) $ first' f 


false :: Lens' (TF a) a
false f = dimap (\(TF a b) -> (a, b)) (\(a, b) -> TF a b) $ second' f 


premap :: (Strong arr, Category arr) => arr i i' -> Moore arr i' o -> Moore arr i o
premap p (Moore m o) = Moore (simple p >>> m) o


ana :: Moore' Bool (TF Int)
ana = layerF (\b -> ((), choose b)) (TF c c)
  where
    c = counter 0
