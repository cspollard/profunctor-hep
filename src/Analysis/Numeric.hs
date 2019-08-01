{-# LANGUAGE FlexibleContexts #-}

module Analysis.Numeric where


import Analysis.Ana
import Data.Profunctor ((:->))


data Num' a b where
  Int' :: Num a => Num' Integer a
  Add' :: Num a => Num' (a, a) a
  Sub' :: Num a => Num' (a, a) a
  Mul' :: Num a => Num' (a, a) a


int' :: (Num a, Member arrs Num') => U2 arrs Integer a
int' = inj2 Int'
{-# INLINE int' #-}


add', sub', mul' :: (Num a, Member arrs Num') => U2 arrs (a, a) a
add' = inj2 Add'
{-# INLINE add' #-}
sub' = inj2 Sub'
{-# INLINE sub' #-}
mul' = inj2 Mul'
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


rat' :: (Fractional a, Member arrs Frac') => U2 arrs Rational a
rat' = inj2 Rat'
{-# INLINE rat' #-}


div' :: (Fractional a, Member arrs Frac') => U2 arrs (a, a) a
div' = inj2 Div'
{-# INLINE div' #-}


runFrac' :: Frac' :-> (->)
runFrac' Rat' = fromRational
runFrac' Div' = uncurry (/)
{-# INLINE runFrac' #-}


repFrac' :: Frac' x y -> String
repFrac' Rat' = "Rat'"
repFrac' Div' = "Div'"
{-# INLINE repFrac' #-}
