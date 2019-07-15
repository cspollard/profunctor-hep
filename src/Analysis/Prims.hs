{-# LANGUAGE PatternSynonyms #-}

module Analysis.Prims where

import Data.Functor.Identity
import Data.Functor.Const
import Data.Proxy
import Data.Bifunctor.Tannen


type Identity2 = Tannen Identity
pattern Identity2 p = Tannen (Identity p)

type Proxy2 = Tannen Proxy
pattern Proxy2 = Tannen Proxy

type Const2 w = Tannen (Const w) Identity2
pattern Const2 w = Tannen (Const w)

type Labeled2 w = Tannen ((,) w)
pattern Labeled2 w p = Tannen (w, p)
