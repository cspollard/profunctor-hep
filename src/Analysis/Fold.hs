module Analysis.Fold where

import Control.Arrow
import Data.Monoid (Sum(..), Product(..))
import Data.Moore
import Data.Profunctor.Optic



accum :: Semigroup m => m -> Moore' m m
accum m = feedback $ liftMoore m (uncurry (<>))
{-# INLINE accum #-}


monoidal :: Monoid m => Moore' m m
monoidal = accum mempty
{-# INLINE monoidal  #-}


counter :: Enum b => b -> Moore' a b
counter b = feedback $ liftMoore b (fst >>> succ)
{-# INLINE counter  #-}


sink :: Moore' a ()
sink = pure ()
{-# INLINE sink  #-}


summed :: Num a => Moore' a a
summed = dimap Sum getSum $ monoidal
{-# INLINE summed  #-}


multiplied :: Num a => Moore' a a
multiplied = dimap Product getProduct $ monoidal
{-# INLINE multiplied #-}
