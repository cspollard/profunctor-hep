{-# LANGUAGE ConstraintKinds #-}


module Analysis.Constrained where


data C2 c c' p a b = (c a, c' b) => C2 { runC2 :: p a b }
