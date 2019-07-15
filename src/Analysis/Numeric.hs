{-# LANGUAGE FlexibleContexts #-}

module Analysis.Numeric where


import Analysis.Free
import Data.Profunctor ((:->))
import Data.Extensible.Sum
import Data.Extensible.Class


data Num' a b where
  Int' :: Num a => Num' Integer a
  Add' :: Num a => Num' (a, a) a
  Sub' :: Num a => Num' (a, a) a
  Mul' :: Num a => Num' (a, a) a


int' :: (Num a, Member arrs Num') => U arrs Integer a
int' = inj Int'
{-# INLINE int' #-}


add', sub', mul' :: (Num a, Member arrs Num') => U arrs (a, a) a
add' = inj Add'
{-# INLINE add' #-}
sub' = inj Sub'
{-# INLINE sub' #-}
mul' = inj Mul'
{-# INLINE mul' #-}


runNum' :: Num' :-> (->)
runNum' Int' = fromInteger
runNum' Add' = uncurry (+)
runNum' Sub' = uncurry (-)
runNum' Mul' = uncurry (*)
{-# INLINE runNum' #-}


repNum' :: Num' x y -> String
repNum' Int' = "Int'"
repNum' Add' = "Add'"
repNum' Sub' = "Sub'"
repNum' Mul' = "Mul'"
{-# INLINE repNum' #-}


data Frac' a b where
  Rat' :: Fractional a => Frac' Rational a
  Div' :: Fractional a => Frac' (a, a) a


rat' :: (Fractional a, Member arrs Frac') => U arrs Rational a
rat' = inj Rat'
{-# INLINE rat' #-}


div' :: (Fractional a, Member arrs Frac') => U arrs (a, a) a
div' = inj Div'
{-# INLINE div' #-}


runFrac' :: Frac' :-> (->)
runFrac' Rat' = fromRational
runFrac' Div' = uncurry (/)
{-# INLINE runFrac' #-}


repFrac' :: Frac' x y -> String
repFrac' Rat' = "Rat'"
repFrac' Div' = "Div'"
{-# INLINE repFrac' #-}
