{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeOperators              #-}

module Lib
    ( someFunc
    ) where

import Control.Applicative
import Control.Monad.Fix
import Control.Monad.Free

import Data.Char (toUpper)
import Data.Hashable
import qualified Data.HashMap.Strict as H
import Data.Maybe (catMaybes)
import Data.List (intersperse)

import qualified GHC.Generics as G

import Prelude hiding (div)

someFunc :: IO ()
someFunc = putStrLn "someFunc"

--------------------------------------------------------------------------------

data Δ a = Empty
  deriving (Eq, G.Generic, Hashable)

data DOM a
  = Text (Δ String)
  | Element String [Props a] [VDOM a]
  deriving (Eq, G.Generic, Hashable)

eqDOM :: DOM a -> DOM b -> Bool
eqDOM = undefined

data DOMF next
  = forall a. Hashable a => View (DOM (Δ a)) (Δ a -> next)
  | forall a b. Recur (VDOM a -> VDOM a) (a -> next)

deriving instance Functor DOMF

newtype VDOM a = VDOM (Free DOMF a)
  deriving stock Functor
  deriving newtype (Applicative, Monad)

instance Hashable a => Hashable (VDOM a) where
  hashWithSalt salt (VDOM (Pure a)) = hashWithSalt salt a
  hashWithSalt salt (VDOM (Free (View dom _))) = hashWithSalt salt dom

instance Eq a => Eq (VDOM a) where
  (VDOM (Pure a)) == (VDOM (Pure b)) = a == b
  (VDOM (Free (View a _))) == (VDOM (Free (View b _))) = a `eqDOM` b

recur :: (VDOM a -> VDOM a) -> VDOM a
recur f = VDOM $ liftF $ Recur f id

view :: Hashable a => DOM (Δ a) -> VDOM (Δ a)
view v = VDOM $ liftF $ View v id

orr :: [VDOM a] -> VDOM a
orr = undefined

--------------------------------------------------------------------------------

data EVDOM = forall a. Hashable a => EVDOM (VDOM a)

instance Eq EVDOM where
  a == b = undefined

instance Hashable EVDOM where
  hashWithSalt salt (EVDOM vdom) = hashWithSalt salt vdom

newtype Name = Name String

for :: [a] -> (a -> b) -> [b]
for = flip map

generate :: Hashable a => H.HashMap EVDOM Name -> VDOM a -> String
generate _ (VDOM (Pure a)) = undefined
generate _ (VDOM (Free (View (Text t) next))) = undefined
generate m (VDOM (Free (View (Element e props chs) next))) = mconcat $ intersperse "\n"
  [ "function _" <> show 0 <> "(remove) {"
  , "  const e = document.createElement('" <> e <> "');"
  , ""
  , mconcat $ for events $ \event -> mconcat $ intersperse "\n"
      [ "e.addEventListener('" <> event <> "', function(e) {"
      , "  "
      , ");"
      ]
  ]
  where
    titleCase (a:as) = toUpper a:as

    events = catMaybes $ for props $ \prop -> case prop of
      Attr _ _ -> Nothing
      Event event -> Just event

    step = generate m (VDOM (next Empty))
    -- step = case H.lookup (EVDOM (VDOM (next Empty))) m of
    --   Nothing -> undefined
    --   Just (Name name) -> name

--------------------------------------------------------------------------------

data Props a
  = Attr String String
  | Event String
  deriving (Show, Eq, G.Generic, Hashable)

attr :: String -> Δ String -> Props a
attr = undefined

onClick :: a -> Props a
onClick = undefined

--------------------------------------------------------------------------------

text :: Hashable a => Δ String -> VDOM (Δ a)
text = view . Text

div :: [Props a] -> [VDOM a] -> VDOM a
div = undefined

enum :: Enum a => Δ a -> (a -> VDOM b) -> VDOM b
enum = undefined

watch :: Δ a -> VDOM b -> VDOM b
watch = undefined

modify :: Δ a -> Δ (a -> b) -> VDOM (Δ ())
modify = undefined

set :: Show a => Δ a -> a -> VDOM (Δ ())
set = undefined

from :: Show a => a -> Δ a
from = undefined

--------------------------------------------------------------------------------

dec :: Δ (Int -> Int)
dec = undefined

inc :: Δ (Int -> Int)
inc = undefined

toString :: Δ a -> Δ String
toString = undefined

data Action = Inc | Dec deriving (Show, G.Generic, Hashable, Enum)

counter v = recur $ \next -> do
  r <- div []
    [ div [ onClick (from Inc) ] [ text (from "-") ]
    , div [] [ text (toString v) ]
    , div [ onClick (from Dec) ] [ text (from "+") ]
    ]

  enum r $ \r -> case r of
    Dec -> do
      modify v dec
      next
    Inc -> do
      modify v inc
      next
