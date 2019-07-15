{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DerivingVia #-}

module Analysis.Prims where

import Data.Functor.Identity
import Data.Functor.Const
import Data.Proxy
import Data.Bifunctor.Tannen
import Data.Profunctor
import Control.Category
import Control.Arrow
import Data.Bifunctor


type Identity2 = Tannen Identity

pattern Identity2 :: p a b -> Identity2 p a b
pattern Identity2 p = Tannen (Identity p)


type Proxy2 = Tannen Proxy

pattern Proxy2 :: Proxy2 p a b
pattern Proxy2 = Tannen Proxy


type Const2 w = Tannen (Const w) Empty2

pattern Const2 :: w -> Const2 w a b
pattern Const2 w = Tannen (Const w)


type Labeled2 w = Tannen ((,) w)

pattern Labeled2 :: w -> p a b -> Labeled2 w p a b
pattern Labeled2 w p = Tannen (w, p)


mapC2 :: (s -> s') -> Const2 s a b -> Const2 s' c d
mapC2 f (Const2 s) = Const2 (f s)
{-# INLINE mapC2 #-}


mapL2 :: (s -> s') -> (p a b -> q c d) -> Labeled2 s p a b -> Labeled2 s' q c d
mapL2 f g (Labeled2 s p) = Labeled2 (f s) (g p)
{-# INLINE mapL2 #-}


forgetL2 :: Labeled2 s p a b -> Const2 s c d
forgetL2 (Labeled2 s _) = Const2 s
{-# INLINE forgetL2 #-}


newtype Empty2 a b = Empty (Const2 () a b)
  deriving (Profunctor, Strong, Choice, Closed, Category, Arrow, Bifunctor) via (Const2 ())
  deriving (Functor) via (Const2 () a)
