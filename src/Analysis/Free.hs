{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}

module Analysis.Free
  ( U, inj, prj, runU, extract, Analysis, Free
  , liftFree, retractFree, runFree, traceFree, hoistFree, runFree', hoistFree'
  , Members
  , module X
  ) where

import Prelude hiding (id, (.))
import Control.Arrow
import Control.Category
import Data.Kind
import Data.Profunctor
import Data.Extensible.Sum
import Data.Extensible.Class as X (Member(..))


type family Members (s :: [k]) (t :: [k]) :: Constraint where
  Members s '[] = ()
  Members s (t ': ts) = (Member s t, Members s ts)


newtype Rel a b rel = Rel { runRel :: rel a b }
newtype U rels a b = U { runUnion :: Rel a b :| rels }


inj :: Member rels rel => rel :-> U rels
inj = U <<< embed <<< Rel
{-# INLINE inj #-}


prj :: (rel a b -> rel' c d) -> (U rels a b -> rel' c d) -> U (rel ': rels) a b -> rel' c d
prj nat nat' = ((nat <<< runRel) <:| (nat' <<< U)) <<< runUnion
{-# INLINE prj #-}


runU :: (rel :-> U rels) -> U (rel ': rels) :-> U rels
runU nat = prj nat id
{-# INLINE runU #-}


extract :: U '[rel] :-> rel
extract = prj id (exhaust <<< runUnion)
{-# INLINE extract #-}


type Analysis rels a b = Free (U rels) a b


data Free p a b where
  Id :: Free p a a
  Arr :: (a -> b) -> Free p a b
  Lift :: p a b -> Free p a b
  Comp :: Free p b c -> Free p a b -> Free p a c
  Par :: Free p a b -> Free p a' b' -> Free p (a, a') (b, b')
  Split :: Free p a b -> Free p a' b' -> Free p (Either a a') (Either b b')




liftFree :: p :-> Free p
liftFree = Lift
{-# INLINE liftFree #-}


-- TODO
-- only one of retract and hoist should be necessary...
retractFree :: (Category p, Choice p, Strong p) => Free p :-> p
retractFree Id = id
retractFree (Arr f) = rmap f id
retractFree (Lift p) = p
retractFree (Comp f f') = retractFree f <<< retractFree f'
retractFree (Par p q) = first' (retractFree p) >>> second' (retractFree q)
retractFree (Split p q) = left' (retractFree p) >>> right' (retractFree q)
{-# INLINE retractFree #-}


hoistFree :: (p :-> q) -> Free p :-> Free q
hoistFree _ Id = Id
hoistFree _ (Arr f) = Arr f
hoistFree nat (Lift p) = Lift (nat p)
hoistFree nat (Comp f f') = Comp (hoistFree nat f) (hoistFree nat f')
hoistFree nat (Par p q) = Par (hoistFree nat p) (hoistFree nat q)
hoistFree nat (Split p q) = Split (hoistFree nat p) (hoistFree nat q)
{-# INLINE hoistFree #-}


runFree :: (Category q, Choice q, Strong q) => (p :-> q) -> Free p :-> q
runFree nat = hoistFree nat >>> retractFree
{-# INLINE runFree #-}


hoistFree' :: Free p a b -> (p :-> q) -> Free q a b
hoistFree' a nat = hoistFree nat a
{-# INLINE hoistFree' #-}


runFree' :: (Category q, Choice q, Strong q) => Free p a b -> (p :-> q) -> q a b    
runFree' a nat = runFree nat a    
{-# INLINE runFree' #-}
    


traceFree :: (forall x y. p x y -> String) -> Free p a b -> String
traceFree _ Id = "Id"
traceFree _ (Arr _) = "Arr"
traceFree f (Lift p) = "Lift (" ++ f p ++ ")"
traceFree f (Comp p p') = "Comp (" ++ traceFree f p ++ ") (" ++ traceFree f p' ++ ")"
traceFree f (Par p q) = "Par (" ++ traceFree f p ++ ") (" ++ traceFree f q ++ ")"
traceFree f (Split p q) = "Split (" ++ traceFree f p ++ ") (" ++ traceFree f q ++ ")"
{-# INLINE traceFree #-}




instance Profunctor (Free p) where
  dimap f g p = arr f >>> p >>> arr g
  {-# INLINE dimap #-}

instance Strong (Free p) where
  first' p = Par p id
  {-# INLINE first' #-}

  second' p = Par id p
  {-# INLINE second' #-}

instance Choice (Free p) where
  left' p = Split p id
  {-# INLINE left' #-}

  right' p = Split id p
  {-# INLINE right' #-}

instance Category (Free p) where
  id = Id
  {-# INLINE id #-}

  (.) = Comp
  {-# INLINE (.) #-}

instance Arrow (Free p) where
  arr = Arr
  {-# INLINE arr #-}

  (***) = Par
  {-# INLINE (***) #-}

instance ArrowChoice (Free p) where
  (+++) = Split
  {-# INLINE (+++) #-}
