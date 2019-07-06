module Analysis.Free where

import Prelude hiding (id, (.))
import Control.Arrow
import Control.Category
import Data.Profunctor
import Data.Profunctor.Traversing



data Free p a b where
  Id :: Free p a a
  Arr :: (a -> b) -> Free p a b
  Lift :: p a b -> Free p a b
  Comp :: Free p b c -> Free p a b -> Free p a c
  Par :: Free p a b -> Free p a' b' -> Free p (a, a') (b, b')
  Split :: Free p a b -> Free p a' b' -> Free p (Either a a') (Either b b')
  Wand :: (forall f. Applicative f => (a -> f b) -> s -> f t) -> Free p a b -> Free p s t


liftFree :: p :-> Free p
liftFree = Lift


retractFree :: (Category p, Traversing p) => Free p :-> p
retractFree Id = id
retractFree (Arr f) = rmap f id
retractFree (Lift p) = p
retractFree (Comp f f') = retractFree f <<< retractFree f'
retractFree (Par p q) = first' (retractFree p) >>> second' (retractFree q)
retractFree (Split p q) = left' (retractFree p) >>> right' (retractFree q)
retractFree (Wand l p) = wander l (retractFree p)


hoistFree :: (p :-> q) -> Free p :-> Free q
hoistFree nat (Lift p) = Lift (nat p)
hoistFree nat (Comp f f') = Comp (hoistFree nat f) (hoistFree nat f')
hoistFree nat (Par p q) = Par (hoistFree nat p) (hoistFree nat q)
hoistFree nat (Split p q) = Split (hoistFree nat p) (hoistFree nat q)
hoistFree nat (Wand l p) = Wand l (hoistFree nat p)
hoistFree _ (Arr f) = Arr f
hoistFree _ Id = Id


runFree :: (Category q, Traversing q) => (p :-> q) -> Free p :-> q
runFree nat = hoistFree nat >>> retractFree


traceFree :: (forall x y. p x y -> String) -> Free p a b -> String
traceFree _ Id = "Id"
traceFree _ (Arr _) = "Arr"
traceFree f (Lift p) = "Lift (" ++ f p ++ ")"
traceFree f (Comp p p') = "Comp (" ++ traceFree f p ++ ") (" ++ traceFree f p' ++ ")"
traceFree f (Par p q) = "Par (" ++ traceFree f p ++ ") (" ++ traceFree f q ++ ")"
traceFree f (Split p q) = "Split (" ++ traceFree f p ++ ") (" ++ traceFree f q ++ ")"
traceFree f (Wand l p) = "Wand (" ++ traceFree f p ++ ")"




instance Profunctor (Free p) where
  dimap f g p = arr f >>> p >>> arr g

instance Strong (Free p) where
  first' p = Par p id
  second' p = Par id p

instance Choice (Free p) where
  left' p = Split p id
  right' p = Split id p

instance Traversing (Free p) where
  wander = Wand

instance Category (Free p) where
  id = Id
  (.) = Comp

instance Arrow (Free p) where
  arr = Arr
  (***) = Par

instance ArrowChoice (Free p) where
  (+++) = Split

