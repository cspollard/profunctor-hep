
module Analysis.Const where

import Prelude hiding (id, (.))
import Control.Category
import Data.Profunctor
import Data.Profunctor.Strong
import Data.Profunctor.Choice
import Data.Profunctor.Traversing
import Data.Profunctor.Closed


newtype Const w a b = Const { getConst :: w }


mapConst :: (w -> w') -> Const w a b -> Const w' c d
mapConst f (Const x) = Const (f x)
{-# INLINE mapConst  #-}


changeConst :: Const w a b -> Const w c d
changeConst = mapConst id
{-# INLINE changeConst  #-}


instance Profunctor (Const w) where
  dimap _ _ = changeConst
  {-# INLINE dimap  #-}


instance Strong (Const w) where
  first' = changeConst
  {-# INLINE first' #-}


instance Choice (Const w) where
  left' = changeConst
  {-# INLINE left' #-}


instance Traversing (Const w) where
  wander _ = changeConst
  {-# INLINE wander  #-}


instance Closed (Const w) where
  closed = changeConst
  {-# INLINE closed  #-}


instance Monoid w => Category (Const w) where
  id = Const mempty
  {-# INLINE id  #-}
  Const w . Const w' = Const $ w' <> w
  {-# INLINE (.)  #-}

