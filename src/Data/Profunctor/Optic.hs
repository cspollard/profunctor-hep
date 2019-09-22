{-# LANGUAGE ConstraintKinds           #-}

module Data.Profunctor.Optic
  ( -- module X
  Optic, Optic', OpticC, OpticC'
  , Simple, Lens, Lens', Traversal, Traversal'
  , starry, hubble
  ) where

import Data.Profunctor -- as X hiding (curry') 
import Data.Profunctor.Traversing -- as X

type Optic p s t a b = p a b -> p s t
type Optic' p s a = p a a -> p s s
type OpticC c s t a b = forall p. c p => Optic p s t a b
type OpticC' c s a = forall p. c p => Optic' p s a

type Simple l s a = l s s a a
type Lens s t a b = OpticC Strong s t a b
type Lens' s a = Simple Lens s a
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
