{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Arrows #-}
{-# OPTIONS_GHC -Wno-orphans #-}


module Data.Moore
  ( Moore, Moore', MooreK
  , extract, update, premap, postmap
  , forceMoore
  , chomp, chomp', chomps, chomps'
  , curry'
  , hoistMoore
  , liftMoore, generalize, simplify
  , feedback, foldlMoore, foldlMooreK
  , TmpA(..)
  ) where


import Prelude hiding (id, (.))
import Data.Foldable (foldlM)
import Data.Profunctor hiding (curry')
import Data.Functor.Identity
import Control.Category
import Control.Arrow
import Data.Both


data Moore p i o
  = forall x.
  Moore
  { _current :: !x
  , update :: !(p i (Moore p i o))
  , _finalize :: !(p x o)
  }


-- this does not require ArrowApply if we use the simpler version of Moore:
-- Moore !o !(p i (Moore p i o))
extract :: ArrowApply p => p (Moore p i o) o
extract = proc m -> do
  Moore x _ f <- id -< m
  app -< (f, x)


premap :: (Profunctor p, Category p) => p i' i -> Moore p i o -> Moore p i' o
premap m (Moore x p f) = Moore x (m >>> rmap (premap m) p) f

postmap :: (Profunctor p, Category p) => p o o' -> Moore p i o -> Moore p i o'
postmap m (Moore x p f) = Moore x (rmap (postmap m) p) (f >>> m)


forceMoore :: ArrowApply p => p (Moore p i o) (Moore p i o)
forceMoore = proc m -> do
  Moore x p f <- id -< m
  o <- app -< (f, x)
  returnA -< Moore o p id

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


liftMoore :: (Category p, Profunctor p) => o -> p i o -> Moore p i o
liftMoore o p = let p' = rmap (\o' -> Moore o' p' id) p in Moore o p' id
{-# INLINE liftMoore #-}


hoistMoore
  :: Profunctor p'
  => (forall a b. p a b -> p' a b) -> Moore p i o -> Moore p' i o
hoistMoore dinat (Moore x p f) = Moore x (rmap (hoistMoore dinat) (dinat p)) (dinat f)
{-# INLINE hoistMoore #-}


generalize :: (Category p, Strong p) => Moore' i o -> Moore p i o
generalize = hoistMoore arr'
{-# INLINE generalize #-}


simplify :: MooreK Identity i o -> Moore' i o
simplify = hoistMoore $ runStar >>> fmap runIdentity
{-# INLINE simplify #-}



feedback
  :: forall p i o.
     (Category p, Strong p)
  => Moore p (o, i) o -> Moore p i o
feedback (Moore x p f) = hoistMoore runTmpA $ Moore x (go x p f) (TmpA f)
  where
    go
      :: x
      -> (p (o, i) (Moore p (o, i) o))
      -> p x o
      -> (TmpA p) i (Moore (TmpA p) i o)
    go x' p' f' = proc i' -> do
      o' <- TmpA f' -< x'
      Moore x'' p'' f'' <- TmpA p' -< (o', i')
      returnA -< Moore x'' (go x'' p'' f'') (TmpA f'')
{-# INLINE feedback #-}


foldlMoore :: Foldable f => Moore' a b -> Moore' (f a) b
foldlMoore = simplify <<< foldlMooreK <<< generalize
{-# INLINE foldlMoore #-}


foldlK :: (Foldable t, Monad m) => Star m (b, a) b -> Star m (b, t a) b
foldlK (Star k) = Star $ \(b, ta) -> foldlM (curry k) b ta
{-# INLINE foldlK #-}


foldlMooreK :: (Foldable f, Monad m) => MooreK m a b -> MooreK m (f a) b
foldlMooreK m@(Moore x _ f) = Moore x (rmap foldlMooreK go) f
  where
    go = foldlK (Star $ \(i, j) -> runStar (update i) j) <<< arr (m,)
{-# INLINE foldlMooreK  #-}




instance Profunctor p => Profunctor (Moore p) where
  dimap g h (Moore x p f) = Moore x (dimap g (dimap g h) p) (rmap h f)
  {-# INLINE dimap #-}


instance Profunctor p => Functor (Moore p i) where
  fmap g (Moore x p f) = Moore x (rmap (rmap g) p) (rmap g f)
  {-# INLINE fmap #-}


instance (Category p, Strong p) => Applicative (Moore p i) where
  pure o = let m = Moore o (arr' (const m)) id in m
  {-# INLINE pure #-}

  Moore xg pg fg <*> Moore xu pu fu =
    hoistMoore runTmpA $ Moore (Both xg xu) (go pg pu) (go' fg fu)

    where
      go qg qu = proc i -> do
        Moore xg' qg' fg' <- TmpA qg -< i
        Moore xu' qu' fu' <- TmpA qu -< i
        returnA -< Moore (Both xg' xu') (go qg' qu') (go' fg' fu')

      go' fg' fu' = proc (Both xg' xu') -> do
        g <- TmpA fg' -< xg'
        u <- TmpA fu' -< xu'
        returnA -< g u
  {-# INLINE (<*>) #-}



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


instance Monad m => Arrow (Star m) where
  arr = arr'
  (***) = par'


instance Monad m => ArrowApply (Star m) where
  app = Star $ \(Star a, x) -> a x
