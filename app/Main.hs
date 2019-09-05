{-# LANGUAGE Arrows #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE DeriveFunctor #-}

module Main where

main :: IO ()
main = return ()
-- import Analysis hiding (VarRel)
-- import Data.Profunctor
-- import Data.Functor.Combinator hiding (generalize)
-- import Control.Arrow
-- import Control.Lens (ix, Traversal', over)
-- import Text.Show.Deriving
-- import Analysis.Variation hiding (VarRel)
-- import qualified Data.HashMap.Monoidal as HM
-- import Data.Bifunctor.Tannen
-- 
-- 
-- histoL
--   :: Monad m
--   => [MooreK m a b]
--   -> MooreK m (Int, a) [b]
-- histoL ms = fmap poop <$> moores (\i -> starry (ix i)) ms
-- 
-- 
-- -- data PF a = PF a a
-- --   deriving Functor
-- -- 
-- -- histoPF
-- --   :: PF (Mealy' a b)
-- --   -> Mealy' (Bool, a)
-- -- histoL ms = fmap poop <$> moores (\i -> starry (ix i)) ms
-- -- 
-- -- 
-- -- 
-- -- ana :: Member rels Mealy' => Analysis rels Int [Int]
-- -- ana = inp $ lmap (,()) go
-- --   where 
-- --     go :: Mealy' (Int, a) [Int]
-- --     go = simplify <<< pop <<< histoL <<< replicate 10 <<< generalize' $ counter 0
--   
-- 
-- 
-- 
-- 
-- inp :: Member rels rel => rel a b -> Analysis rels a b
-- inp = liftAna <<< inj2
-- 
-- 
-- type Var = Variation String
-- type AnaOut = Free (Var :+: [])
-- 
-- deriveShow1 ''(:+:)
-- 
-- 
-- type VarRel p = Tannen Var p
-- type AnaRel p = Tannen AnaOut p
-- 
-- 
-- data Region i p s a = Region (p a a -> p (i, s) s) s
-- 
-- data Event = Event Float Float
-- 
-- vars' :: Member rels (VarRel rel) => rel a b -> [(String, rel a b)] -> Analysis rels a b
-- vars' x y = inp <<< Tannen $ vars x y
-- 
-- 
-- 
-- ana
--   :: Members rels [VarRel (->), (->)]
--   => Analysis rels Event Int
-- ana = proc (Event ept mpt) -> do
--   vept <- vars' id [("elcalib", (+5))] -< ept
--   vmpt <- vars' id [("mucalib", (+2))] -< mpt
--   inp $ floor -< vept + vmpt
--   -- $ moores (\i -> starry $ ix i) (replicate 20 $ generalize counter) -< (bin, ())
-- 
-- 
-- 
-- -- how do I build something like
-- -- Free [Pure m, Free (Vars (Pure m) [])]
-- -- ??
-- -- how about like this
-- -- where for variations the optic will always be everything
-- 
-- selective
--   :: (Functor f, ArrowApply p, Strong p)
--   => (a -> Optic' p (f (Moore p w b)) (Moore p w b))
--   -> f (Moore p w b)
--   -> Moore p (a, w) (f b)
-- selective trav ms = (fmap.fmap) poop <<< feedback' $ Moore (simple go) ms
--   where
--     go = proc (ms', (a, w)) -> app -< (trav a (lmap (,w) chomp), ms')
-- {-# INLINE selective #-}
-- 
-- 
-- hoistTannen :: (forall a b. f (p a b) -> g (q a b)) -> Tannen f p :-> Tannen g q
-- hoistTannen nat = Tannen <<< nat <<< runTannen
-- 
-- 
-- hoistTannen1 :: (f ~> g) -> Tannen f p :-> Tannen g p
-- hoistTannen1 = hoistTannen
-- 
-- 
-- hoistTannen2 :: Functor f => (p :-> q) -> Tannen f p :-> Tannen f q
-- hoistTannen2 nat = hoistTannen (fmap nat)
-- 
-- 
-- ana' :: Mealy' Event (AnaOut Int)
-- ana' =
--   sequenceA <<< runTannen
--   $ runAna' ana
--     $ (hoistTannen1 (inject <<< inR) <<< hoistTannen2 simple)
--       <<| (hoistTannen1 (inject <<< inL) <<< hoistTannen2 pop)
--       <<| (hoistTannen1 (inject <<< inL) <<< hoistTannen2 simple)
--       <<| (Tannen <<< pure <<< arr)
--       <<| extract2
-- 
-- 
-- main :: IO ()
-- main = do
--   let a' = runMealy ana' (Event 20 25)
--   print $ poop a'
--   let a'' = chomp (a', (Event 40 25))
--   print $ poop a''
