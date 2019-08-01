{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeApplications #-}

module Analysis.Union
  ( U1, inj1, prj1, run1, extract1
  , U2, inj2, prj2, run2, extract2
  , Members
  , module X
  ) where


import Data.Kind
import Data.Extensible.Sum
import Data.Extensible.Class as X (Member(..))
import Control.Arrow


type f ~> g = forall a. f a -> g a
type f :-> g = forall a b. f a b -> g a b


type family Members (s :: [k]) (t :: [k]) :: Constraint where
  Members s '[] = ()
  Members s (t ': ts) = (Member s t, Members s ts)


newtype L1 a f = L1 { runL1 :: f a }
newtype L2 a b f = L2 { runL2 :: f a b }


newtype U1 fs a = U1 { runU1 :: L1 a :| fs }
newtype U2 fs a b = U2 { runU2 :: L2 a b :| fs }


inj1 :: Member xs x => x ~> U1 xs
inj1 = L1 >>> embed >>> U1
{-# INLINE inj1 #-}

inj2 :: Member xs x => x :-> U2 xs
inj2 = L2 >>> embed >>> U2
{-# INLINE inj2 #-}


prj1 :: (x a -> c) -> (U1 xs a -> c) -> U1 (x : xs) a -> c
prj1 nat nat' = runU1 >>> ((runL1 >>> nat) <:| (U1 >>> nat'))
{-# INLINE prj1 #-}

prj2 :: (x a b -> c) -> (U2 xs a b -> c) -> U2 (x : xs) a b -> c
prj2 nat nat' = runU2 >>> ((runL2 >>> nat) <:| (U2 >>> nat'))
{-# INLINE prj2 #-}


run1 :: (x ~> U1 xs) -> U1 (x : xs) ~> U1 xs
run1 nat = prj1 nat id
{-# INLINE run1 #-}

run2 :: (x :-> U2 xs) -> U2 (x : xs) :-> U2 xs
run2 nat = prj2 nat id
{-# INLINE run2 #-}


extract1 :: U1 '[x] ~> x
extract1 = prj1 id (runU1 >>> exhaust)
{-# INLINE extract1 #-}

extract2 :: U2 '[x] :-> x
extract2 = prj2 id (runU2 >>> exhaust)
{-# INLINE extract2 #-}
