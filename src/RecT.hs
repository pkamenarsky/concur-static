{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeOperators              #-}

module RecT where

import Control.Applicative
import Control.Monad.Free

import Data.Char (toUpper)
import Data.Hashable
import qualified Data.HashMap.Strict as H
import Data.Sequence (Seq, (|>), (<|))
import qualified Data.Sequence as Seq
import Data.Maybe (catMaybes)
import Data.List (intersperse)

import qualified GHC.Generics as G

import Prelude hiding (break)

data Δ a = E

data RecF a next = forall b. Step a (Δ b -> next) | Recurse

deriving instance Functor (RecF a)

newtype RecT a b = RecT (Free (RecF a) b)
  deriving (Functor, Applicative, Monad)

step :: a -> RecT a (Δ b)
step a = RecT $ liftF $ Step a id

unroll :: RecT a (Δ b) -> Seq a
unroll (RecT (Pure _)) = Seq.empty
unroll (RecT (Free (Step a next))) = a <| unroll (RecT (next E))

break :: Eq a => RecT a (Δ b) -> Seq a -> Seq a
break (RecT (Pure _)) as = as
break (RecT (Free (Step a next))) as = if Seq.take (Seq.length as + 1) recurse == as |> a
  then as |> a
  else break (RecT (next E)) (as |> a)
  where
    recurse = unroll (RecT (next E))

test :: Int -> RecT Int (Δ a)
test x = do
  step 4
  step 5
  step 6
  step 7
  step 8
  test x
  step 9
  test x
