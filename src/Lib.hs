{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TupleSections              #-}

module Lib where

import Control.Monad (replicateM)
import Control.Monad.Fix
import Control.Monad.Free
import qualified Control.Monad.Trans.State.Strict as ST

import Data.Char (toUpper)
import Data.String
import Data.Maybe (catMaybes)
import Data.List (intersperse)

import Prelude hiding (div)

someFunc :: IO ()
someFunc = putStrLn "someFunc"

--------------------------------------------------------------------------------

data Δ a where
  Empty :: Δ a
  Var   :: Name -> Δ a
  Raw   :: String -> Δ a
  Apply :: Δ (a -> b) -> Δ a -> Δ b

toJS :: Δ a -> String
toJS Empty = error "Empty"
toJS (Var (Name a)) = "window._" <> a
toJS (Raw a) = a
toJS (Apply f a) = toJS f <> "(" <> toJS a <> ")"

binary :: Num a => String -> Δ (a -> b -> c)
binary op = Raw $ "(function(a) { return function(b) { return a " <> op <> " b; } })"

instance Num a => Num (Δ a) where
  a + b = Apply (Apply (binary "+") a) b
  a * b = Apply (Apply (binary "*") a) b
  abs a = Apply (Raw "(function(a} { return Math.abs(a); })") a
  signum a = Apply (Raw "(function(a} { return Math.sign(a); })") a
  fromInteger a = Raw (show a)
  negate a = Apply (Raw "(function(a} { return -a; })") a

instance IsString (Δ String) where
  fromString = Raw

(.<) :: Num a => Δ a -> Δ a -> Δ Bool
a .< b = Apply (Apply (binary "<") a) b

(.>) :: Num a => Δ a -> Δ a -> Δ Bool
a .> b = Apply (Apply (binary "<") a) b

a :: Δ Int
a = 5 + 6

data DOM a
  = Text (Δ String)
  | ConstText String
  | Element String [Props a] [VDOM a]

data DOMF next
  = forall a. View (DOM (Δ a)) (Δ a -> next)
  | forall a. Recur (VDOM a -> VDOM a) (a -> next)
  | forall a. Call Name
  | forall a b. Enum a => Enum (Δ a) (a -> VDOM (Δ b)) (Δ b -> next)

deriving instance Functor DOMF

newtype VDOM a = VDOM (Free DOMF a)
  deriving (Functor, Applicative, Monad)

recur :: (VDOM a -> VDOM a) -> VDOM a
recur f = VDOM $ liftF $ Recur f id

view :: DOM (Δ a) -> VDOM (Δ a)
view v = VDOM $ liftF $ View v id

enum :: Enum a => Δ a -> (a -> VDOM (Δ b)) -> VDOM (Δ b)
enum v f = VDOM $ liftF $ Enum v f id

--------------------------------------------------------------------------------

newtype Name = Name String deriving (Eq)

instance Show Name where
  show (Name name) = name

newName :: ST.State (Int, a) Name
newName = ST.state $ \(i, a) -> (Name $ "_" <> show i, (i + 1, a))

generate :: VDOM a -> ST.State (Int, [(Name, String)]) Name
generate (VDOM (Pure a)) = pure $ Name "done"
generate (VDOM (Free (Call name))) = pure name
generate (VDOM (Free (Recur vdom next))) = mfix $ \name ->
  generate (vdom (VDOM $ liftF $ Call name))
generate (VDOM (Free (Enum v f next))) = do
  undefined
generate (VDOM (Free (View (ConstText t) next))) = do
  name <- newName

  ST.modify $ \(i, m) -> (i, (name, mkBody name):m)
  pure name

  where
    mkBody name = mconcat $ intersperse "\n"
      [ "function " <> show name <> "(_next, parent, index) {"
      , "  const e = document.createTextNode(" <> show t <> ");"
      , "  parent.insertBefore(e, parent.childNodes[index]);"
      , "}"
      ]

generate (VDOM (Free (View (Element e props children) next))) = do
  chNames <- sequence [ generate child | child <- children ]

  name <- newName
  nextName <- generate (VDOM (next Empty))

  body <- mkBody name nextName chNames

  ST.modify $ \(i, m) -> (i, (name, body):m)
  pure name

  where
    mkBody name nextName chNames = pure $ mconcat $ intersperse "\n"
      [ "function " <> show name <> "(kill, parent, index) {"
      , "  const e = document.createElement('" <> e <> "');"
      , ""
      , "  const suicide = function() {"
      , "    parent.removeChild(e);"
      , if nextName == Name "done"
          then "    kill();"
          else "    " <> show nextName <> "(kill, parent, index);"
      , "  };"
      , ""
      , "  parent.insertBefore(e, parent.childNodes[index]);"
      , ""
      , mconcat $ intersperse "\n"
          [ mconcat $ intersperse "\n"
              [ "  e.addEventListener('" <> event <> "', suicide);"
              ]
          | Event event <- props
          ]
      , ""
      , mconcat $ intersperse "\n"
          [ "  " <> show chName <> "(suicide, e, " <> show index <> ");"
          | (index, chName) <- zip [0..] chNames
          ]
      , "}"
      ]

generateModule :: VDOM a -> String
generateModule vdom = mconcat $ intersperse "\n"
  [ "<meta charset=\"utf-8\">"
  , "<!doctype html>"
  , "<html>"
  , "<head>"
  , "  <title>~</title>"
  , "</head>"
  , "<body style=\"margin: 0\">"
  , "</body>"
  , "<script>"
  , mconcat $ intersperse "\n\n"
      [ body
      | (_, body) <- fns
      ]
  , show startName <> "(end, document.body, 0);"
  , "function end() {}"
  , ""
  , "</script>"
  , "</html>"
  ]
  where
    (startName, (_, fns)) = ST.runState (generate vdom) (0, [])

--------------------------------------------------------------------------------

data Props a
  = Attr String String
  | Event String

attr :: String -> Δ String -> Props a
attr = undefined

onClick :: a -> Props a
onClick a = Event "click"

--------------------------------------------------------------------------------

text :: Δ String -> VDOM (Δ a)
text = view . Text

text' :: String -> VDOM (Δ a)
text' = view . ConstText

div :: [Props (Δ a)] -> [VDOM (Δ a)] -> VDOM (Δ a)
div props children = VDOM $ liftF $ View (Element "div" props children) id

watch :: Δ a -> VDOM b -> VDOM b
watch = undefined

modify :: Δ a -> Δ (a -> b) -> VDOM (Δ ())
modify = undefined

set :: Show a => Δ a -> a -> VDOM (Δ ())
set = undefined

from :: Show a => a -> Δ a
from = undefined

--------------------------------------------------------------------------------

test1 = do
  replicateM 3 $ do
    div [ onClick (from 1) ] [ text' "1", text' "11", text' "111" ]
    div [ onClick (from 2) ] [ text' "2" ]
  pure Empty

test2 x
  | x > 10 = pure Empty
  | otherwise = do
      div [ onClick (from ()) ] [ text' (show x) ]
      test2 (x + 1)

test3 = recur $ \next -> do
  div [] [ div [ onClick (from ()) ] [ text' "A" ], test1 ]
  div [] [ text' "B", test2 0, test2 0 ]
  next

sidebar = recur $ \next -> do
  div [ onClick (from ()) ] [ text' "BLACK" ] 
  div [ onClick (from ()) ] [ text' "WHITE" ] 
  next

test4 = div [] [ sidebar, test3 ]

--------------------------------------------------------------------------------

dec :: Δ (Int -> Int)
dec = undefined

inc :: Δ (Int -> Int)
inc = undefined

toString :: Δ a -> Δ String
toString = undefined

data Action = Inc | Dec deriving (Show, Enum)

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
