module Analysis.Weight where

import Data.Monoid (Sum(..), Product(..))
import Control.Arrow

-- TODO
-- give names to SFs using e.g. Compose (MonoidalHashMap String) First

type Weighted = (,) (Sum Float)

type Scaled = (,) (Product Float)

type SFRel = Kleisli ((,) (Product Float))


sfToWeight :: Scaled a -> Weighted a
sfToWeight (Product x, y) = (Sum x, y)
