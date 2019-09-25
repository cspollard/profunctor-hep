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
  , apply, layer, layerF, layerEither, layerBoth
  , WrapA(..)
  ) where


import Prelude hiding (id, (.))
import Data.Foldable (foldlM)
import Data.Profunctor hiding (curry')
import Data.Profunctor.Optic
import Data.Functor.Identity
import Control.Category
import Control.Arrow
import Control.Applicative (liftA2)
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
  o <- extract -< m
  returnA -< Moore o (update m) id


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
feedback (Moore x p f) = hoistMoore runWrapA $ Moore x (go x p f) (WrapA f)
  where
    go
      :: x
      -> (p (o, i) (Moore p (o, i) o))
      -> p x o
      -> (WrapA p) i (Moore (WrapA p) i o)
    go x' p' f' = proc i' -> do
      o' <- WrapA f' -< x'
      Moore x'' p'' f'' <- WrapA p' -< (o', i')
      returnA -< Moore x'' (go x'' p'' f'') (WrapA f'')
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


apply :: Applicative f => f (Moore' i o) -> Moore' (f i) (f o)                                                                 
apply ms = (fmap.fmap) extract . feedback $ liftMoore ms (uncurry $ liftA2 chomps)                                                          
{-# INLINE apply #-}


layer
  :: (Strong p, ArrowApply p)
  => p i' (i, Optic' p s (Moore p i x))
  -- ^ instructions to transform an index into container access and a new index
  -> s
  -- ^ the container of accumulators
  -> Moore p i' s
  -- ^ the total accumulator
layer idx ms = feedback $ liftMoore ms go
  where
    go = proc (ms', i') -> do
      (i, opt) <- idx -< i'
      p <- chomps' -< i
      app -< (opt p, ms')
{-# INLINE layer #-}


-- | functorial version of layer
layerF
  :: (Mapping p, ArrowApply p, Functor f)
  => p i' (i, Optic' p (f (Moore p i o)) (Moore p i o))
  -- ^ instructions to transform an index into container access and a new index
  -> f (Moore p i o)
  -- ^ the container of accumulators
  -> Moore p i' (f o)
  -- ^ the total accumulator
layerF idx ms = postmap (map' extract) $ layer idx ms
{-# INLINE layerF #-}


layerEither
  :: (Strong p, ArrowChoice p, ArrowApply p)
  => Both (Moore p a c) (Moore p b d)
  -> Moore p (Either a b) (Both c d)
layerEither both = postmap go $ layerEither' both
  where
    go = arr toTuple >>> (extract *** extract) >>> arr fromTuple 
{-# INLINE layerEither #-}


layerEither'
  :: (Strong p, ArrowChoice p, ArrowApply p)
  => Both (Moore p a c) (Moore p b d)
  -> Moore p (Either a b) (Both (Moore p a c) (Moore p b d))
layerEither' both = feedback $ liftMoore both go
  where
    l' = proc l -> do
      a <- app -< (chomps', l)
      returnA -< _1 a

    r' = proc r -> do
      a <- app -< (chomps', r)
      returnA -< _2 a

    go = proc (m, i) -> do
      a <- l' ||| r' -< i
      app -< (a, m)
{-# INLINE layerEither' #-}


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


layerBoth
  :: (Strong p, ArrowApply p)
  => Both (Moore p a c) (Moore p b d)
  -> Moore p (Both a b) (Both c d)
layerBoth both = postmap go $ layerBoth' both
  where
    go = arr toTuple >>> (extract *** extract) >>> arr fromTuple 
{-# INLINE layerBoth #-}



instance (Category p, Strong p, Semigroup o) => Semigroup (Moore p i o) where
  (<>) = liftA2 (<>)


instance (Category p, Strong p, Monoid o) => Monoid (Moore p i o) where
  mempty = pure mempty


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
    hoistMoore runWrapA $ Moore (Both xg xu) (go pg pu) (go' fg fu)

    where
      -- ghc panics without the following two type signatures.
      go :: p i (Moore p i (a -> b)) -> p i (Moore p i a) -> WrapA p i (Moore (WrapA p) i b)
      go qg qu = proc i -> do
        Moore xg' qg' fg' <- WrapA qg -< i
        Moore xu' qu' fu' <- WrapA qu -< i
        returnA -< Moore (Both xg' xu') (go qg' qu') (go' fg' fu')

      go' :: p x (a -> b) -> p x' a -> WrapA p (Both x x') b
      go' fg' fu' = proc (Both xg' xu') -> do
        g <- WrapA fg' -< xg'
        u <- WrapA fu' -< xu'
        returnA -< g u
  {-# INLINE (<*>) #-}



-- every strong category is an arrow
newtype WrapA p a b = WrapA { runWrapA :: p a b }
  deriving (Category, Profunctor, Strong) via p


instance (Category p, Strong p) => Arrow (WrapA p) where
  arr f = rmap f id
  m *** m' = first' m <<< second' m'


arr' :: (Category p, Strong p) => (a -> b) -> p a b
arr' = runWrapA <<< arr
{-# INLINE arr' #-}


par' :: (Category p, Strong p) => p a b -> p c d -> p (a, c) (b, d)
par' m m' = runWrapA $ WrapA m *** WrapA m'
{-# INLINE par' #-}


instance Monad m => Arrow (Star m) where
  arr = arr'
  (***) = par'


instance Monad m => ArrowApply (Star m) where
  app = Star $ \(Star a, x) -> a x
