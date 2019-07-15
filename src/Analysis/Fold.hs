{-# LANGUAGE Arrows           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms  #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE TupleSections    #-}


module Analysis.Fold where

import Data.Tuple (swap)
import           Control.Arrow
import           Control.Category
import Data.Profunctor hiding (curry')
import           Analysis.MealyMoore
import           Prelude                    hiding (id, (.))



accum :: Semigroup m => m -> Moore' m m
accum m = feedback' $ Moore (arr $ uncurry (<>)) m


monoidal :: Monoid m => Moore' m m
monoidal = accum mempty


counter :: Enum b => b -> Moore' a b
counter b = feedback' $ Moore (arr $ \(b', _) -> succ b') b


sink :: Moore' a ()
sink = pure ()


prepend :: (Category arr, Strong arr) => Mealy arr a b -> Moore arr b c -> Moore arr a c
prepend m' (Moore m o) = Moore (m' >>> m) o


type T arr a s = arr a a -> arr s s

-- we could probably generalize this even more...
histo
  :: (ArrowApply arr, Profunctor arr)
  => (i -> T arr (Moore arr i o) s)
  -> s
  -> Moore arr i s
histo l ms = Moore (Mealy $ rmap (histo l) go) ms
  where
    go = proc i -> do
      p <- app -< (curry' $ chomp <<< arr swap, i)
      app -< (l i p, ms)


histoM
  :: Monad m
  => (i -> (MooreK m i o -> m (MooreK m i o)) -> s -> m s)
  -> s
  -> MooreK m i s
histoM l = histo (lensToK <<< l)


lensToK
  :: ((a -> f b) -> (s -> f t))
  -> Kleisli f a b
  -> Kleisli f s t
lensToK l (Kleisli k) = Kleisli $ l k
