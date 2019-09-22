-- {-# LANGUAGE DataKinds #-}
-- {-# LANGUAGE DeriveFunctor #-}
-- {-# LANGUAGE DeriveFoldable #-}
-- {-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE Arrows #-}

module Analysis.Test where

import Prelude hiding (id, (.))
import Control.Arrow
import Data.Both
import Control.Category
import Data.Profunctor hiding (curry')
import Data.Moore
import Analysis.Fold
import Data.Profunctor.Optic
import Control.Applicative (liftA2)
import Data.Comps
import Data.Functor.Compose
import Data.Bifunctor


_Compose :: OpticC Profunctor (Compose f g a) (Compose h k b) (f (g a)) (h (k b))
_Compose = dimap (\(Compose fga) -> fga) (\fga -> Compose fga)



asdf :: Moore' Int a -> Moore' Int b -> Moore' Int (Both a b)
asdf = liftA2 Both


