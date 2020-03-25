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
  deriving (Eq, Show)

data RecF a next = forall b. Step a (Δ b -> next) | Recurse

deriving instance Functor (RecF a)

newtype RecT a b = RecT (Free (RecF a) b)
  deriving (Functor, Applicative, Monad)

step :: a -> RecT a (Δ b)
step a = RecT $ liftF $ Step a id

unroll :: RecT a (Δ b) -> [a]
unroll (RecT (Pure _)) = []
unroll (RecT (Free (Step a next))) = a:unroll (RecT (next E))

break :: Eq a => RecT a (Δ b) -> [Either a (Δ b)] -> [Either a (Δ b)]
break (RecT (Pure b)) as = as <> [Right b]
break (RecT (Free (Step a next))) as = if take (length as + 1) recurse `subst` (as <> [Left a])
  then as <> [Left a]
  else break (RecT (next E)) (as <> [Left a])
  where
    recurse = map Left $ unroll (RecT (next E))

    subst [] [] = False
    subst xs ys@(b:bs) = if xs == ys then True else subst (if null xs then [] else init xs) bs

test3 :: Int -> RecT Int (Δ Int)
test3 x = do
  step 20
  step 21
  step 22
  pure E

test2 :: Int -> RecT Int (Δ Int)
test2 x = do
  step 10
  step 11
  test3 5
  test2 9
  step 12

test :: Int -> RecT Int (Δ Int)
test x = do
  step 4
  step 5
  step 4
  step 5
  step 6
  test2 7
  step 7
  step 8
  pure E
