{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE Arrows #-}
{-# OPTIONS_GHC -Wno-orphans #-}


module Data.Moore where

import Prelude hiding (id, (.))
import Data.Foldable (foldlM)
import Data.Profunctor hiding (curry')
import Data.Functor.Identity
import Control.Category
import Control.Arrow


data Moore p i o
  = Moore
  { current :: !o
  , update :: !(p i (Moore p i o))
  }



type Moore' = Moore (->)
type MooreK m = Moore (Star m)


chomp :: ArrowApply p => p (Moore p i o, i) (Moore p i o)
chomp = first (arr update) >>> app
{-# INLINE chomp #-}


chomp' :: ArrowApply p => p (i, Moore p i o) (Moore p i o)
chomp' = arr swap >>> chomp
  where
    swap (a, b) = (b, a)
{-# INLINE chomp'  #-}


chomps :: ArrowApply p => p (Moore p i o) (p i (Moore p i o))
chomps = curry' chomp
{-# INLINE chomps  #-}


chomps' :: ArrowApply p => p i (p (Moore p i o)  (Moore p i o))
chomps' = curry' chomp'
{-# INLINE chomps'  #-}


curry' :: Arrow p => p (i, i') o -> p i (p i' o)
curry' p = proc i -> do
  let go = proc i' -> do p -< (i, i')
  returnA -< go
{-# INLINE curry' #-}


hoistMoore
  :: Profunctor p'
  => (forall a b. p a b -> p' a b) -> Moore p i o -> Moore p' i o
hoistMoore f (Moore o m) = Moore o $ rmap (hoistMoore f) (f m)
{-# INLINE hoistMoore #-}


instance Profunctor p => Profunctor (Moore p) where
  dimap f g (Moore o m) = Moore (g o) $ dimap f (dimap f g) m
  {-# INLINE dimap #-}


instance Profunctor p => Functor (Moore p i) where
  fmap f (Moore o m) = Moore (f o) $ rmap (rmap f) m
  {-# INLINE fmap #-}


instance (Category p, Strong p) => Applicative (Moore p i) where
  pure o = let m = Moore o $ arr' (const m) in m
  {-# INLINE pure #-}

  Moore f mf <*> Moore x mx = hoistMoore runTmpA $ Moore (f x) (go mf mx)
    where
      go nf nx = proc i -> do
        Moore f' nf' <- TmpA nf -< i
        Moore x' nx' <- TmpA nx -< i
        returnA -< Moore (f' x') (go nf' nx')
  {-# INLINE (<*>) #-}


simple :: Profunctor p => o -> p i o -> Moore p i o
simple o p = let p' = rmap (\o' -> Moore o' p') p in Moore o p'
{-# INLINE simple #-}


generalize :: (Category p, Strong p) => Moore' i o -> Moore p i o
generalize = hoistMoore arr'
{-# INLINE generalize #-}


simplify :: MooreK Identity i o -> Moore' i o
simplify = hoistMoore $ runStar >>> fmap runIdentity
{-# INLINE simplify #-}



feedback :: (Category p, Strong p) => Moore p (o, i) o -> Moore p i o
feedback (Moore o m) = hoistMoore runTmpA $ Moore o (go o m)
  where
    go o' m' = proc i -> do
      Moore o'' m'' <- TmpA m' -< (o', i)
      returnA -< Moore o'' (go o'' m'')
{-# INLINE feedback #-}


foldlMoore :: Foldable f => Moore' a b -> Moore' (f a) b
foldlMoore = simplify <<< foldlMooreK <<< generalize
{-# INLINE foldlMoore #-}


foldlK :: (Foldable t, Monad m) => Star m (b, a) b -> Star m (b, t a) b
foldlK (Star k) = Star $ \(b, ta) -> foldlM (curry k) b ta
{-# INLINE foldlK #-}


foldlMooreK :: (Foldable f, Monad m) => MooreK m a b -> MooreK m (f a) b
foldlMooreK m@(Moore o _) = Moore o (rmap foldlMooreK go)
  where
    go = foldlK (Star $ \(x, y) -> runStar (update x) y) <<< arr (m,)
{-# INLINE foldlMooreK  #-}



instance Monad m => Arrow (Star m) where
  arr = arr'
  (***) = par'


instance Monad m => ArrowApply (Star m) where
  app = Star $ \(Star a, x) -> a x


-- every strong category is an arrow
newtype TmpA p a b = TmpA { runTmpA :: p a b }
  deriving (Category, Profunctor, Strong) via p

instance (Category p, Strong p) => Arrow (TmpA p) where
  arr f = rmap f id
  m *** m' = first' m <<< second' m'


arr' :: (Category p, Strong p) => (a -> b) -> p a b
arr' = runTmpA <<< arr
{-# INLINE arr' #-}


par' :: (Category p, Strong p) => p a b -> p c d -> p (a, c) (b, d)
par' m m' = runTmpA $ TmpA m *** TmpA m'
{-# INLINE par' #-}
