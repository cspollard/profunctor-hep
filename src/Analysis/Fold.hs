{-# LANGUAGE Arrows           #-}
{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms  #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE TupleSections    #-}
{-# LANGUAGE DeriveFunctor    #-}
{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveTraversable    #-}


module Analysis.Fold where

import           Control.Arrow
import Data.Profunctor hiding (curry')
import Data.Profunctor.Traversing
import Data.Monoid (Sum(..), Product(..))
import           Analysis.MealyMoore
import           Prelude                    hiding (id, (.))



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
type OpticC' c s a = forall p. c p => Optic' p s a

type Simple l s a = l s s a a
type Lens s t a b = OpticC Strong s t a b
type Lens' s a = Simple Lens s a
type Traversal s t a b = OpticC Traversing s t a b
type Traversal' s a = Simple Traversal s a


over :: Optic p s t a b -> p a b -> p s t
over = ($)
{-# INLINE over  #-}


-- view a star through a lens
starry :: ((a -> f b) -> (s -> f t)) -> Optic (Star f) s t a b
starry l (Star f) = Star (l f)
{-# INLINE starry #-}


-- cast a starry lens into a functional one
hubble :: Optic (Star f) s t a b -> (a -> f b) -> s -> f t
hubble o g = runStar (o (Star g))
{-# INLINE hubble #-}


layer
  :: (Strong p, ArrowApply p)
  => p i' (i, Optic' p s (Moore p i o))
  -- ^ instructions to transform an index into container access and a new index
  -> s
  -- ^ the container of accumulators
  -> Moore p i' s
  -- ^ the total accumulator
layer idx ms = Moore (feedback (simple go) ms) ms
  where
    go = proc (ms', i') -> do
      (i, opt) <- idx -< i'
      p <- curry' chomp' -< i
      app -< (opt p, ms')
{-# INLINE layer #-}


-- | functorial version of layer
layerF
  :: (Strong p, ArrowApply p, Functor f)
  => p i' (i, Optic' p (f (Moore p i o)) (Moore p i o))
  -- ^ instructions to transform an index into container access and a new index
  -> f (Moore p i o)
  -- ^ the container of accumulators
  -> Moore p i' (f o)
  -- ^ the total accumulator
layerF idx ms = fmap output <$> layer idx ms
{-# INLINE layerF #-}
