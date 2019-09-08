{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Analysis.Meas where

import Data.Extensible.Product
import Data.Extensible.Inclusion
import Data.Functor.Identity
import Data.Extensible.Class
import Data.Tagged


type Meas supp range = Identity :* supp -> range


type Members zs = Forall (Member zs)


together
  :: (Semigroup range, Members zs xs, Members zs ys)
  => Meas xs range -> Meas ys range -> Meas zs range
together p p' m = p (shrink m) <> p' (shrink m)


newtype LogProb a = LogProb { runLogProb :: a }
  deriving Show

instance Num a => Semigroup (LogProb a) where
  LogProb a <> LogProb b = LogProb (a + b)

instance Num a => Monoid (LogProb a) where
  mempty = LogProb 0


data NP1
data NP2


test :: LogProb Float
test = together (go @NP1) (go @NP2) $ a <: b <: nil
  where
    go :: Identity :* '[Tagged x a] -> a
    go = unTagged . runIdentity . hlookup leadership

    a :: Identity (Tagged NP1 (LogProb Float))
    a = Identity (Tagged (LogProb 0.1))

    b :: Identity (Tagged NP2 (LogProb Float))
    b = Identity (Tagged (LogProb 0.1))
