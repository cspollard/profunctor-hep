{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Analysis.Union
 ( Union, U, Member(..), Members(..), (::+::)(..), extract, sumI, sumU, runU
 ) where

import GHC.TypeLits (TypeError, ErrorMessage(..))
import Unsafe.Coerce (unsafeCoerce)
import Data.Profunctor
import Control.Arrow ((>>>))
import Data.Kind (Constraint)


-- | Open union is a strong sum (existential with an evidence).
data Union (r :: [* -> * -> *]) a b where
  Union :: {-# UNPACK #-} !Word -> p a b -> Union r a b


type U = Union

unsafeInj :: Word -> t a b -> Union r a b
unsafeInj = Union
{-# INLINE unsafeInj #-}

unsafePrj :: Word -> Union r a b -> Maybe (t a b)
unsafePrj n (Union n' x)
  | n == n'   = Just (unsafeCoerce x)
  | otherwise = Nothing
{-# INLINE unsafePrj #-}


newtype P (t :: * -> * -> *) (r :: [* -> * -> *])  = P {unP :: Word}


class FindElem (t :: * -> * -> *) (r :: [* -> * -> *]) where
  elemNo :: P t r

instance FindElem t (t ': r) where
  elemNo = P 0

instance {-# OVERLAPPABLE #-} FindElem t r => FindElem t (t' ': r) where
  elemNo = P $ 1 + unP (elemNo :: P t r)

class IfNotFound (t :: * -> * -> *) (r :: [* -> * -> *]) (w :: [* -> * -> *])

instance TypeError ('Text "‘" ':<>: 'ShowType t
                    ':<>: 'Text "’ is not a member of the type-level list"
                    ':$$: 'Text "  ‘" ':<>: 'ShowType w ':<>: 'Text "’"
                    ':$$: 'Text "In the constraint ("
                    ':<>: 'ShowType (Member t w) ':<>: 'Text ")")
    => IfNotFound t '[] w

instance IfNotFound t (t ': r) w

instance {-# OVERLAPPABLE #-} IfNotFound t r w => IfNotFound t (t' ': r) w

instance {-# INCOHERENT #-} IfNotFound t r w


class FindElem arr arrs => Member (arr :: * -> * -> *) arrs where
  inj :: arr :-> Union arrs

  prj :: Union arrs a b -> Maybe (arr a b)


type family Members effs effs' :: Constraint where
  Members (eff ': effs) effs' = (Member eff effs', Members effs effs')
  Members '[] effs' = ()


instance (FindElem t r, IfNotFound t r r) => Member t r where
  inj = unsafeInj $ unP (elemNo :: P t r)
  {-# INLINE inj #-}

  prj = unsafePrj $ unP (elemNo :: P t r)
  {-# INLINE prj #-}


data (::+::) arr arr' a b where
  L2 :: arr a b -> (::+::) arr arr' a b
  R2 :: arr' a b -> (::+::) arr arr' a b


decomp :: Union (t ': r) a b -> Either (Union r a b) (t a b)
decomp (Union 0 a) = Right $ unsafeCoerce a
decomp (Union n a) = Left  $ Union (n - 1) a
{-# INLINE [2] decomp #-}


sumI :: (arr :-> arr'') -> (arr' :-> arr'') -> arr ::+:: arr' :-> arr''
sumI f _ (L2 arr) = f arr
sumI _ g (R2 arr') = g arr'
{-# INLINE sumI #-}


sumU :: Union (t ': r) :-> Union r ::+:: t
sumU = decomp >>> either L2 R2
{-# INLINE sumU #-}


runU :: (arr :-> U arrs) -> U (arr ': arrs) :-> U arrs
runU nat = sumU >>> sumI id nat
{-# INLINE runU #-}


extract :: U '[ arr ] :-> arr
extract (Union _ a) = unsafeCoerce a
{-# INLINE extract #-}