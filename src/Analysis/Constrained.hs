{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ConstraintKinds #-}


module Analysis.Constrained where


newtype C2 c c' p = C2 { runC2 :: forall x y. (c x, c' y) => p x y }
