{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}

module Data.Comps where

import Data.Functor.Classes


data Comps (fs :: [* -> *]) a where
  Done :: !a -> Comps '[] a
  More :: !(f (Comps fs a)) -> Comps (f : fs) a


instance Show1 (Comps '[]) where
  liftShowsPrec sp _ d (Done x) = showsUnaryWith sp "Done" d x


instance (Show1 (Comps fs), Show1 f) => Show1 (Comps (f : fs)) where
  liftShowsPrec sp sp' d (More fs) =
    showString "More "
    . liftShowsPrec (liftShowsPrec sp sp') (liftShowList sp sp') d fs


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

instance (Traversable (Comps fs), Traversable f) => Traversable (Comps (f :fs)) where
  traverse g (More fs) = More <$> traverse (traverse g) fs
