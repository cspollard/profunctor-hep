{-# LANGUAGE Arrows           #-}
{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms  #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE TupleSections    #-}


module Analysis.Fold where

import           Control.Arrow
import Data.Profunctor hiding (curry')
import Data.Monoid (Sum(..), Product(..))
import           Analysis.MealyMoore
import           Prelude                    hiding (id, (.))
import qualified Control.Lens as L



accum :: Semigroup m => m -> Moore' m m
accum m = feedback' $ Moore (arr $ uncurry (<>)) m
{-# INLINE accum  #-}


monoidal :: Monoid m => Moore' m m
monoidal = accum mempty
{-# INLINE monoidal  #-}


counter :: Enum b => b -> Moore' a b
counter b = feedback' $ Moore (arr $ fst >>> succ) b
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



type Optic p s t a b = p a b -> p s t
type Optic' p s a = p a a -> p s s
type OpticC c s t a b = forall p. c p => Optic p s t a b

type Simple l s a = l s s a a
type Lens s t a b = OpticC Strong s t a b
type Lens' s a = Simple Lens s a


starry :: ((a -> f b) -> (s -> f t)) -> Optic (Star f) s t a b
starry l (Star f) = Star (l f)
{-# INLINE starry #-}


containedMoore
  :: (Strong p, ArrowApply p)
  => (a -> Optic' p s (Moore p b w))
    -- ^ instructions to access parts of a container given an index
  -> s
    -- ^ the container of accumulators
  -> Moore p (a, b) s
    -- ^ the total accumulator
containedMoore trav s = feedback' $ Moore (simple go) s
  where
    go = proc (ms', (a, w)) -> app -< (trav a (lmap (,w) chomp), ms')
{-# INLINE containedMoore #-}


-- | a histogram is a structure of accumulators that 
--    accumulates at an index.
histogram
  :: (Monad m, L.Ixed s, L.Index s ~ a, L.IxValue s ~ MooreK m b w)
  => s
    -- ^ the container of accumulators
  -> MooreK m (a, b) s
    -- ^ the total accumulator
histogram = containedMoore (starry <<< L.ix)
{-# INLINE histogram  #-}
