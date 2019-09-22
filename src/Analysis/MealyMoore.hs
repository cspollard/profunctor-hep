{-# LANGUAGE Arrows                    #-}
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE DerivingVia               #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE PatternSynonyms           #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# OPTIONS_GHC -Wno-orphans #-}


module Analysis.MealyMoore where

import           Control.Arrow
import           Control.Category
import           Data.Functor.Identity
import           Data.Profunctor hiding (curry')
import           Prelude               hiding (id, (.))
import Data.Foldable (foldlM)


newtype Mealy arr i o = Mealy { runMealy :: arr i (Moore arr i o) }

-- TODO
-- need to think about strictness
data Moore arr i o
  = Moore
  { update :: !(Mealy arr i o)
  , output :: !o
  }


type Mealy' = Mealy (->)
type MealyK m = Mealy (Star m)
type Moore' = Moore (->)
type MooreK m = Moore (Star m)


instance Profunctor arr => Profunctor (Mealy arr) where
  dimap f g (Mealy m) = Mealy $ dimap f (dimap f g) m
  {-# INLINE dimap  #-}


instance Profunctor arr => Profunctor (Moore arr) where
  dimap f g (Moore m o) = Moore (dimap f g m) (g o)
  {-# INLINE dimap  #-}


instance Strong arr => Strong (Mealy arr) where
  first' (Mealy m) = Mealy (rmap go $ first' m)
    where
      go (Moore m' o, c) = Moore (first' m') (o, c)
  {-# INLINE first'  #-}


instance Choice arr => Choice (Mealy arr) where
  left' (Mealy m) = meal
    where
      meal = Mealy (rmap go $ left' m)

      go (Left (Moore m' o)) = Moore (left' m') (Left o)
      go (Right c) = Moore meal (Right c)
  {-# INLINE left'  #-}




instance (Category arr, Strong arr) => Category (Mealy arr) where
  id = Mealy (rmap (\i -> Moore id i) id)
  {-# INLINE id  #-}

  Mealy f . Mealy g =
    Mealy <<< runTmpA $ proc i -> do
      Moore m o <- TmpA g -< i
      Moore m' o' <- TmpA f -< o
      returnA -< Moore (m' . m) o'
  {-# INLINE (.) #-}


instance (Category arr, Strong arr) => Arrow (Mealy arr) where
  arr f = rmap f id
  {-# INLINE arr  #-}
  m *** m' = first' m <<< second m'
  {-# INLINE (***)  #-}



instance Profunctor arr => Functor (Mealy arr i) where
  fmap = rmap
  {-# INLINE fmap  #-}

instance Profunctor arr => Functor (Moore arr i) where
  fmap = rmap
  {-# INLINE fmap  #-}


instance (Category arr, Strong arr) => Applicative (Mealy arr i) where
  pure o = rmap (const o) id
  {-# INLINE pure  #-}

  Mealy mf <*> Mealy mx = Mealy <<< runTmpA $ (TmpA mf &&& TmpA mx) >>> arr (uncurry (<*>))
  {-# INLINE (<*>) #-}


instance (Category arr, Strong arr) => Applicative (Moore arr i) where
  pure o = Moore (pure o) o
  {-# INLINE pure  #-}

  Moore mf f <*> Moore mx x = Moore (mf <*> mx) (f x)
  {-# INLINE (<*>) #-}



simple :: (Category arr, Strong arr) => arr i o -> Mealy arr i o
simple f = m
  where
    m = Mealy $ f >>> arr' (Moore m)
{-# INLINE simple #-}


chomp :: ArrowApply arr => arr (Moore arr i o, i) (Moore arr i o)
chomp = first (arr $ update >>> runMealy) >>> app
{-# INLINE chomp  #-}


chomp' :: ArrowApply arr => arr (i, Moore arr i o) (Moore arr i o)
chomp' = arr swap >>> chomp
  where
    swap (a, b) = (b, a)
{-# INLINE chomp'  #-}


chomps :: ArrowApply arr => arr (Moore arr i o) (arr i (Moore arr i o))
chomps = curry' chomp
{-# INLINE chomps  #-}


chomps' :: ArrowApply arr => arr i (arr (Moore arr i o)  (Moore arr i o))
chomps' = curry' chomp'
{-# INLINE chomps'  #-}



curry' :: Arrow arr => arr (i, i') o -> arr i (arr i' o)
curry' p = proc i -> do
  let go = proc i' -> do p -< (i, i')
  returnA -< go
{-# INLINE curry' #-}


lower :: (Category arr, Strong arr) => Mealy arr i o -> arr i o
lower (Mealy m) = m >>> arr' output
{-# INLINE lower  #-}


feedback' :: (Category arr, Strong arr) => Moore arr (o, i) o -> Moore arr i o
feedback' (Moore m o) = Moore (feedback m o) o
{-# INLINE feedback' #-}


feedback :: (Category arr, Strong arr) => Mealy arr (o, i) o -> o -> Mealy arr i o
feedback (Mealy m) o = Mealy $ dimap (o,) feedback' m
{-# INLINE feedback #-}


generalize :: (Category arr, Strong arr) => Mealy' i o -> Mealy arr i o
generalize = hoistMealy arr'
{-# INLINE generalize  #-}


generalize' :: (Category arr, Strong arr) => Moore' i o -> Moore arr i o
generalize' = hoistMoore arr'
{-# INLINE generalize' #-}


hoistMealy
  :: (Category arr, Strong arr)
  => (forall a b. arr a b -> arr' a b) -> Mealy arr i o -> Mealy arr' i o
hoistMealy f (Mealy m) = Mealy (f $ m >>> arr' (hoistMoore f))
{-# INLINE hoistMealy  #-}


hoistMoore
  :: (Category arr, Strong arr)
  => (forall a b. arr a b -> arr' a b) -> Moore arr i o -> Moore arr' i o
hoistMoore f (Moore m o) = Moore (hoistMealy f m) o
{-# INLINE hoistMoore  #-}


foldlMoore :: Foldable f => Moore' a b -> Moore' (f a) b
foldlMoore = simplify' <<< foldlMooreK <<< generalize' 
{-# INLINE foldlMoore  #-}


foldlK :: (Foldable t, Monad m) => Star m (b, a) b -> Star m (b, t a) b
foldlK (Star k) = Star $ \(b, ta) -> foldlM (curry k) b ta
{-# INLINE foldlK  #-}


foldlMooreK :: (Foldable f, Monad m) => MooreK m a b -> MooreK m (f a) b
foldlMooreK m@(Moore _ o) = Moore (Mealy $ rmap foldlMooreK go) o
  where
    go = foldlK chomp <<< arr (m,)
{-# INLINE foldlMooreK  #-}


simplify :: MealyK Identity i o -> Mealy' i o
simplify = hoistMealy $ runStar >>> fmap runIdentity
{-# INLINE simplify  #-}


simplify' :: MooreK Identity i o -> Moore' i o
simplify' = hoistMoore $ runStar >>> fmap runIdentity
{-# INLINE simplify' #-}


instance Monad m => Arrow (Star m) where
  arr = arr'
  (***) = par'

instance Monad m => ArrowApply (Star m) where
  app = Star $ \(Star a, x) -> a x


-- every strong category is an arrow
newtype TmpA arr a b = TmpA { runTmpA :: arr a b }
  deriving (Category, Profunctor, Strong) via arr

instance (Category arr, Strong arr) => Arrow (TmpA arr) where
  arr f = rmap f id
  m *** m' = first' m <<< second' m'


arr' :: (Category p, Strong p) => (a -> b) -> p a b
arr' = runTmpA <<< arr
{-# INLINE arr' #-}


par' :: (Category p, Strong p) => p a b -> p c d -> p (a, c) (b, d)
par' m m' = runTmpA $ TmpA m *** TmpA m'
{-# INLINE par' #-}
