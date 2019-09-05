{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}

module Analysis.Test where

import Prelude hiding (id, (.))
import Control.Arrow
import Control.Category
import Data.Profunctor
import Analysis.MealyMoore
import Analysis.Fold
import Data.Comps
import Data.Functor.Classes
import Control.Lens.At



data TF a = TF a a deriving (Functor, Foldable, Traversable, Show)

instance Show1 TF where
  liftShowsPrec sp _ d (TF x y) = showsBinaryWith sp sp "TF" d x y


type instance Index (TF a) = Bool
type instance IxValue (TF a) = a


instance Ixed (TF a) where
  ix b = hubble $ choose b


choose :: Bool -> Lens' (TF a) a
choose True = true
choose False = false


true :: Lens' (TF a) a
true f = dimap (\(TF a b) -> (a, b)) (\(a, b) -> TF a b) $ first' f 


false :: Lens' (TF a) a
false f = dimap (\(TF a b) -> (a, b)) (\(a, b) -> TF a b) $ second' f 


premap :: (Strong arr, Category arr) => arr i i' -> Moore arr i' o -> Moore arr i o
premap p (Moore m o) = Moore (simple p >>> m) o

liftComps :: Functor f => f a -> Comps '[f] a
liftComps xs = More $ Done <$> xs


ana :: Monad m => MooreK m (Int :. Bool) (Comps '[[], TF] Int)
ana =
  fmap More
  . layerF (arr $ \(i :. is) -> (is, ix' i))
  . replicate 5
  . fmap liftComps
  $ layerF (arr $ \i -> ((), ix' i)) (tf c)
  where
    tf x = TF x x
    c = generalize' (counter 0)
