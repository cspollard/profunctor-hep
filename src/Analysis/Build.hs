{-# LANGUAGE FlexibleContexts #-}

module Analysis.Build where

import Analysis.Union
import Data.Profunctor ((:->))
import Analysis.Const


data Num' a b where
  Int' :: Num a => Num' Integer a
  Add' :: Num a => Num' (a, a) a
  Sub' :: Num a => Num' (a, a) a
  Mul' :: Num a => Num' (a, a) a


int' :: (Num a, Member Num' arrs) => U arrs Integer a
int' = inj Int'


add', sub', mul' :: (Num a, Member Num' arrs) => U arrs (a, a) a
add' = inj Add'
sub' = inj Sub'
mul' = inj Mul'


runNum' :: Num' :-> (->)
runNum' Int' = fromInteger
runNum' Add' = uncurry (+)
runNum' Sub' = uncurry (-)
runNum' Mul' = uncurry (*)


repNum' :: Num' x y -> String
repNum' Int' = "Int'"
repNum' Add' = "Add'"
repNum' Sub' = "Sub'"
repNum' Mul' = "Mul'"


data Frac' a b where
  Rat' :: Fractional a => Frac' Rational a
  Div' :: Fractional a => Frac' (a, a) a


rat' :: (Fractional a, Member Frac' arrs) => U arrs Rational a
rat' = inj Rat'


div' :: (Fractional a, Member Frac' arrs) => U arrs (a, a) a
div' = inj Div'


runFrac' :: Frac' :-> (->)
runFrac' Rat' = fromRational
runFrac' Div' = uncurry (/)


repFrac' :: Frac' x y -> String
repFrac' Rat' = "Rat'"
repFrac' Div' = "Div'"


