{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds #-}

module Data.Comps where

import Data.Functor.Classes
import Analysis.Fold
import Data.Profunctor
import Control.Lens.At


data Comps (fs :: [* -> *]) a where
  Done :: !a -> Comps '[] a
  More :: !(f (Comps fs a)) -> Comps (f : fs) a


_Done :: OpticC Profunctor (Comps '[] a) (Comps '[] b) a b
_Done = dimap (\(Done x) -> x) (\y -> Done y)


_More :: OpticC Profunctor (Comps (f : fs) a) (Comps (f : fs) b) (f (Comps fs a)) (f (Comps fs b))
_More = dimap (\(More fs) -> fs) (\fs -> More fs)


instance Show1 (Comps '[]) where
  liftShowsPrec sp _ d (Done x) = showsUnaryWith sp "Done" d x


instance (Show1 (Comps fs), Show1 f) => Show1 (Comps (f : fs)) where
  liftShowsPrec sp sp' d (More fs) =
    showsUnaryWith (liftShowsPrec (liftShowsPrec sp sp') (liftShowList sp sp')) "More" d fs


instance Show a => Show (Comps '[] a) where
  showsPrec = liftShowsPrec showsPrec showList

instance (Show1 f, Show1 (Comps fs), Show a) => Show (Comps (f : fs) a) where
  showsPrec = liftShowsPrec showsPrec showList


instance Functor (Comps '[]) where
  fmap g (Done x) = Done (g x)

instance (Functor (Comps fs), Functor f) => Functor (Comps (f : fs)) where
  fmap g (More fs) = More $ fmap g <$> fs


instance Foldable (Comps '[]) where
  foldMap g (Done a) = g a

instance (Foldable (Comps fs), Foldable f) => Foldable (Comps (f :fs)) where
  foldMap g (More fs) = foldMap (foldMap g) fs


instance Traversable (Comps '[]) where
  traverse g (Done a) = Done <$> g a

instance (Traversable (Comps fs), Traversable f) => Traversable (Comps (f : fs)) where
  traverse g (More fs) = More <$> traverse (traverse g) fs



infixr 3 :.
data a :. b = a :. b
data Z = Z


type instance Index (Comps '[] a) = Z
type instance Index (Comps (f : fs) a) = Index (f (Comps fs a)) :. Index (Comps fs a)

type instance IxValue (Comps fs a) = a

instance Ixed (Comps '[] a) where
  ix _ = hubble _Done

instance (IxValue (f (Comps fs a)) ~ Comps fs a, Ixed (f (Comps fs a)), Ixed (Comps fs a))
  => Ixed (Comps (f : fs) a) where
  ix (i :. is) = hubble _More . ix i . ix is

ix' :: (Ixed m, Applicative f) => Index m -> Optic' (Star f) m (IxValue m)
ix' = starry . ix
