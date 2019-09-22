{-# LANGUAGE Arrows           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms  #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE TupleSections    #-}
{-# LANGUAGE DeriveFunctor    #-}
{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveTraversable    #-}


module Analysis.Fold where

import Control.Arrow
import Data.Monoid (Sum(..), Product(..))
import Data.Moore
import Data.Profunctor.Optic
import Data.Profunctor
import Data.Both



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




-- layer
--   :: (Strong p, ArrowApply p)
--   => p i' (i, Optic' p s (Moore p i x))
--   -- ^ instructions to transform an index into container access and a new index
--   -> s
--   -- ^ the container of accumulators
--   -> Moore p i' s
--   -- ^ the total accumulator
-- layer idx ms = feedback $ liftMoore ms go
--   where
--     go = proc (ms', i') -> do
--       (i, opt) <- idx -< i'
--       p <- chomps' -< i
--       app -< (opt p, ms')
-- {-# INLINE layer #-}
-- 
-- 
-- -- | functorial version of layer
-- layerF
--   :: (Mapping p, ArrowApply p, Functor f)
--   => p i' (i, Optic' p (f (Moore p i o)) (Moore p i o))
--   -- ^ instructions to transform an index into container access and a new index
--   -> f (Moore p i o)
--   -- ^ the container of accumulators
--   -> Moore p i' (f o)
--   -- ^ the total accumulator
-- layerF idx ms = postmap (map' extract) $ layer idx ms
-- {-# INLINE layerF #-}


-- layerEither
--   :: (Strong p, ArrowChoice p, ArrowApply p)
--   => Both (Moore p a c) (Moore p b d)
--   -> Moore p (Either a b) (Both c d)
-- layerEither both = postmap go $ layerEither' both
--   where
--     go = arr toTuple >>> (extract *** extract) >>> arr fromTuple 
-- {-# INLINE layerEither' #-}
-- 
-- 
-- layerEither'
--   :: (Strong p, ArrowChoice p, ArrowApply p)
--   => Both (Moore p a c) (Moore p b d)
--   -> Moore p (Either a b) (Both (Moore p a c) (Moore p b d))
-- layerEither' both = feedback $ liftMoore both go
--   where
--     l' = proc l -> do
--       a <- app -< (chomps', l)
--       returnA -< _1 a
-- 
--     r' = proc r -> do
--       a <- app -< (chomps', r)
--       returnA -< _2 a
-- 
--     go = proc (m, i) -> do
--       a <- l' ||| r' -< i
--       app -< (a, m)
-- {-# INLINE layerEither #-}


layerBoth'
  :: forall p a b c d. (Strong p, ArrowApply p)
  => Both (Moore p a c) (Moore p b d)
  -> Moore p (Both a b) (Both (Moore p a c) (Moore p b d))
layerBoth' both = feedback $ liftMoore both go
  where
    go :: p (Both (Moore p a c) (Moore p b d), Both a b) (Both (Moore p a c) (Moore p b d))
    go = arr transp >>> (chomp *** chomp) >>> arr (uncurry Both)

    transp :: (Both (Moore p a c) (Moore p b d), Both a b) -> ((Moore p a c, a), (Moore p b d, b))
    transp (Both x y, Both z w) = ((x, z), (y, w))
{-# INLINE layerBoth' #-}


-- layerBoth
--   :: (Strong p, ArrowApply p)
--   => Both (Moore p a c) (Moore p b d)
--   -> Moore p (Both a b) (Both c d)
-- layerBoth both = postmap go $ layerBoth' both
-- {-# INLINE layerBoth #-}
