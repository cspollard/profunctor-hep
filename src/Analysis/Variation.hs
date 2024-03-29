{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}


module Analysis.Variation where


import Data.Hashable
import qualified Data.HashMap.Monoidal as HM
import Data.Semigroup (First(..))
import Control.Arrow ((<<<))
import Data.Profunctor
import Text.Show.Deriving


type VarRel s = Star (Variation s)


data Variation s a = Variation !a !(HM.MonoidalHashMap s (First a))
  deriving (Functor, Foldable, Traversable, Show)

deriveShow1 ''First
deriveShow1 ''HM.MonoidalHashMap
deriveShow1 ''Variation


vars :: (Eq s, Hashable s) => a -> [(s, a)] -> Variation s a
vars n v = Variation n <<< HM.fromList $ fmap First <$> v


instance (Eq s, Hashable s) => Applicative (Variation s) where
  pure x = Variation x mempty

  Variation f fs <*> Variation x xs =
    Variation (f x) $ (fmap.fmap) f xs <> (fmap.fmap) ($ x) fs


instance (Eq s, Hashable s) => Monad (Variation s) where
  Variation x xs >>= f =
    let Variation fx fxs = f x

        collapse n (Variation y ys) = (n, maybe (First y) id (HM.lookup n ys))

        fxs' =
          HM.fromList
          <<< fmap (\(i, y) -> collapse i $ getFirst y) -- [(s, First a)]
          <<< HM.toList -- [(s, First (Variation a))]
          $ (fmap.fmap) f xs -- HM (First (Variation a))

    in Variation fx (fxs' <> fxs)
