{-# LANGUAGE ConstraintKinds           #-}

module Data.Profunctor.Optic
  ( module X
  , Optic, Optic', OpticC, OpticC'
  , Simple, Iso, Iso', Lens, Lens', Prism, Prism', Traversal, Traversal'
  , starry, hubble, view, views
  ) where

import Data.Profunctor as X hiding (curry') 
import Data.Profunctor.Traversing as X
import Data.Functor.Const


type Optic p s t a b = p a b -> p s t
type Optic' p s a = p a a -> p s s
type OpticC c s t a b = forall p. c p => Optic p s t a b
type OpticC' c s a = forall p. c p => Optic' p s a

type Simple l s a = l s s a a

type Iso s t a b = OpticC Profunctor s t a b
type Iso' s a = Simple Iso s a

type Lens s t a b = OpticC Strong s t a b
type Lens' s a = Simple Lens s a

type Prism s t a b = OpticC Choice s t a b
type Prism' s a = Simple Prism s a

type Traversal s t a b = OpticC Traversing s t a b
type Traversal' s a = Simple Traversal s a


-- view a star through a lens
starry :: ((a -> f b) -> (s -> f t)) -> Optic (Star f) s t a b
starry l (Star f) = Star (l f)
{-# INLINE starry #-}


-- cast a starry lens into a functional one
hubble :: Optic (Star f) s t a b -> (a -> f b) -> s -> f t
hubble o g = runStar (o (Star g))
{-# INLINE hubble #-}


view :: Optic' (Star (Const a)) s a -> s -> a
view p = getConst . runStar (p (Star $ \a -> Const a))
{-# INLINE view #-}


views :: Optic' (Star (Const a)) s a -> (a -> b) -> s -> b
views p f = f . view p
{-# INLINE views #-}
