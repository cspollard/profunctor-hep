{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DeriveGeneric #-}

module Data.Both where

import GHC.Generics
import Data.Bitraversable
import Data.Bifunctor
import Data.Bifoldable
import Data.Biapplicative
import Data.Bifunctor.Join
import Data.Bifunctor.Biff
import Data.Bifunctor.Tannen
import Data.Profunctor
import Data.Profunctor.Optic
import Data.Functor.Identity


data Both a b = Both !a !b deriving (Generic, Show)


instance Bitraversable Both where
  bitraverse f g (Both a b) = Both <$> f a <*> g b
  {-# INLINE bitraverse  #-}

instance Bifunctor Both where
  bimap = bimapDefault
  {-# INLINE bimap  #-}

instance Bifoldable Both where
  bifoldMap = bifoldMapDefault
  {-# INLINE bifoldMap  #-}

instance Biapplicative Both where
  bipure = Both 
  {-# INLINE bipure  #-}
  Both f g <<*>> Both a b = Both (f a) (g b)
  {-# INLINE (<<*>>)  #-}


instance Functor (Both a) where
  fmap = second
  {-# INLINE fmap  #-}

instance Monoid s => Applicative (Both s) where
  pure = Both mempty
  {-# INLINE pure  #-}

  Both s f <*> Both t x = Both (s <> t) (f x)
  {-# INLINE (<*>)  #-}

instance Monoid s => Monad (Both s) where
  Both s a >>= f = let Both t b = f a in Both (s <> t) b
  {-# INLINE (>>=)  #-}


toTuple :: Both a b -> (a, b)
toTuple (Both a b) = (a, b)

fromTuple :: (a, b) -> Both a b
fromTuple (a, b) = Both a b


_1 :: Lens' (Both a b) a
_1 f = dimap (\(Both a b) -> (a, b)) (\(a, b) -> Both a b) $ first' f 


_2 :: Lens' (Both a b) b
_2 f = dimap (\(Both a b) -> (a, b)) (\(a, b) -> Both a b) $ second' f 


type TF = Join Both

type Cut f = Tannen (Both String) (Biff Both Identity f)


type PassFail f g = Biff Both f g


pattern TF :: a -> a -> TF a
pattern TF x y = Join (Both x y)


choose :: Bool -> Lens' (TF a) a
choose True = true
choose False = false


true :: Lens' (TF a) a
true f = dimap (\(TF a b) -> (a, b)) (\(a, b) -> TF a b) $ first' f 


false :: Lens' (TF a) a
false f = dimap (\(TF a b) -> (a, b)) (\(a, b) -> TF a b) $ second' f 
