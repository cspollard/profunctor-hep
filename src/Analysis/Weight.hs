module Analysis.Weight where

import Data.Profunctor
import Data.Profunctor.Traversing


-- TODO
-- give names to SFs using e.g. Compose (MonoidalHashMap String) First

type ASDF p a w s = p (s, a, w) s -- a function from a to a view into s

type Hist a w s = forall p. Strong p => ASDF p a w s

type Hists a w s = forall p. Traversing p => ASDF p a w s

