{-# LANGUAGE PatternSynonyms #-}

module Data.Both where

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


data Both a b = Both !a !b deriving Show


instance Bitraversable Both where
  bitraverse f g (Both a b) = Both <$> f a <*> g b

instance Bifunctor Both where
  bimap = bimapDefault

instance Bifoldable Both where
  bifoldMap = bifoldMapDefault

instance Biapplicative Both where
  bipure = Both 
  Both f g <<*>> Both a b = Both (f a) (g b)

instance Functor (Both a) where
  fmap = second


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
