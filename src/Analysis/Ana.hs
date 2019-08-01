module Analysis.Ana
  ( U2, inj2, prj2, run2, extract2, Analysis, Ana
  , liftAna, retractAna, runAna, traceAna, hoistAna, runAna', hoistAna'
  , Members
  , module X
  ) where


import Prelude hiding (id, (.))
import Analysis.Union
import Control.Arrow
import Control.Category
import Data.Profunctor
import Data.Extensible.Class as X (Member(..))


type Analysis rels a b = Ana (U2 rels) a b


-- This type simply supports Strong and Category, e.g. Arrow.
data Ana p a b where
  Id :: Ana p a a
  Arr :: (a -> b) -> Ana p a b
  Lift :: p a b -> Ana p a b
  Comp :: Ana p b c -> Ana p a b -> Ana p a c
  Par :: Ana p a b -> Ana p a' b' -> Ana p (a, a') (b, b')



liftAna :: p :-> Ana p
liftAna = Lift
{-# INLINE liftAna #-}


-- TODO
-- only one of retract and hoist should be necessary...
retractAna :: (Category p, Strong p) => Ana p :-> p
retractAna Id = id
retractAna (Arr f) = rmap f id
retractAna (Lift p) = p
retractAna (Comp f f') = retractAna f <<< retractAna f'
retractAna (Par p q) = first' (retractAna p) >>> second' (retractAna q)
{-# INLINE retractAna #-}


hoistAna :: (p :-> q) -> Ana p :-> Ana q
hoistAna _ Id = Id
hoistAna _ (Arr f) = Arr f
hoistAna nat (Lift p) = Lift (nat p)
hoistAna nat (Comp f f') = Comp (hoistAna nat f) (hoistAna nat f')
hoistAna nat (Par p q) = Par (hoistAna nat p) (hoistAna nat q)
{-# INLINE hoistAna #-}


runAna :: (Category q, Strong q) => (p :-> q) -> Ana p :-> q
runAna nat = hoistAna nat >>> retractAna
{-# INLINE runAna #-}


hoistAna' :: Ana p a b -> (p :-> q) -> Ana q a b
hoistAna' a nat = hoistAna nat a
{-# INLINE hoistAna' #-}


runAna' :: (Category q, Strong q) => Ana p a b -> (p :-> q) -> q a b    
runAna' a nat = runAna nat a    
{-# INLINE runAna' #-}
    


traceAna :: (forall x y. p x y -> String) -> Ana p a b -> String
traceAna _ Id = "Id"
traceAna _ (Arr _) = "Arr"
traceAna f (Lift p) = "Lift (" ++ f p ++ ")"
traceAna f (Comp p p') = "Comp (" ++ traceAna f p ++ ") (" ++ traceAna f p' ++ ")"
traceAna f (Par p q) = "Par (" ++ traceAna f p ++ ") (" ++ traceAna f q ++ ")"
-- traceAna f (Split p q) = "Split (" ++ traceAna f p ++ ") (" ++ traceAna f q ++ ")"
{-# INLINE traceAna #-}



instance Profunctor (Ana p) where
  dimap f g p = arr f >>> p >>> arr g
  {-# INLINE dimap #-}

instance Strong (Ana p) where
  first' p = Par p id
  {-# INLINE first' #-}

  second' p = Par id p
  {-# INLINE second' #-}

instance Category (Ana p) where
  id = Id
  {-# INLINE id #-}

  (.) = Comp
  {-# INLINE (.) #-}

instance Arrow (Ana p) where
  arr = Arr
  {-# INLINE arr #-}

  (***) = Par
  {-# INLINE (***) #-}
