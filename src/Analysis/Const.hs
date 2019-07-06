
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


changeConst :: Const w a b -> Const w c d
changeConst = mapConst id


instance Profunctor (Const w) where
  dimap _ _ = changeConst


instance Strong (Const w) where
  first' = changeConst


instance Choice (Const w) where
  left' = changeConst


instance Traversing (Const w) where
  wander _ = changeConst


instance Closed (Const w) where
  closed = changeConst


instance Monoid w => Category (Const w) where
  id = Const mempty
  Const w . Const w' = Const $ w' <> w

