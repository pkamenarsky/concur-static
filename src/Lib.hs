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
  = Text String
  | Element String [Props a] [VDOM a]

data DOMF next
  = forall a. (Enum a, Bounded a) => View (DOM a) (a -> next)
  | forall a. (Enum a, Bounded a) => Loop (VDOM a -> VDOM a) (a -> next)
  | Call Name

deriving instance Functor DOMF

newtype VDOM a = VDOM (Free DOMF a)
  deriving (Functor, Applicative, Monad)

loop :: Enum a => Bounded a => (VDOM a -> VDOM a) -> VDOM a
loop f = VDOM $ liftF $ Loop f id

view :: Enum a => Bounded a => DOM a -> VDOM a
view v = VDOM $ liftF $ View v id

--------------------------------------------------------------------------------

newtype Name = Name String deriving (Eq)

instance Show Name where
  show (Name name) = name

newName :: ST.State (Int, a) Name
newName = ST.state $ \(i, a) -> (Name $ "_" <> show i, (i + 1, a))

enumAll :: Enum a => Bounded a => t a -> [a]
enumAll _ = map toEnum [minBound..maxBound]

generate :: Enum a => Bounded a => VDOM a -> ST.State (Int, [(Name, String)]) Name
generate (VDOM (Pure a)) = pure $ Name "done"
generate (VDOM (Free (Call name))) = pure name
generate (VDOM (Free (Loop vdom next))) = mfix $ \name ->
  generate (vdom (VDOM $ liftF $ Call name))
generate (VDOM (Free (View (Text t) next))) = do
  name <- newName

  ST.modify $ \(i, m) -> (i, (name, mkBody name):m)
  pure name

  where
    mkBody name = mconcat $ intersperse "\n"
      [ "function " <> show name <> "(_, parent, index) {"
      , "  const e = document.createTextNode(" <> show t <> ");"
      , "  parent.insertBefore(e, parent.childNodes[index]);"
      , "}"
      ]

generate (VDOM (Free (View dom@(Element e props children) next))) = do
  chNames <- sequence [ generate child | child <- children ]

  name <- newName
  nextName <- generate (VDOM $ next $ head $ enumAll dom)

  nexts <- traverse generate (map (VDOM . next) (enumAll dom))

  body <- mkBody name nextName chNames

  ST.modify $ \(i, m) -> (i, (name, body):m)
  pure name

  where
    mkBody name nextName chNames = pure $ mconcat $ intersperse "\n"
      [ "function " <> show name <> "(kill, parent, index) {"
      , "  const e = document.createElement('" <> e <> "');"
      , ""
      , "  const suicide = function(r) {"
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
              [ "  e.addEventListener('" <> event <> "', suicide(" <> show (fromEnum value) <> "));"
              ]
          | Event event value <- props
          ]
      , ""
      , mconcat $ intersperse "\n"
          [ "  " <> show chName <> "(suicide, e, " <> show index <> ");"
          | (index, chName) <- zip [0..] chNames
          ]
      , "}"
      ]

generateModule :: Enum a => Bounded a => VDOM a -> String
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
  | Event String a

attr :: String -> Δ String -> Props a
attr = undefined

onClick :: a -> Props a
onClick a = Event "click" a

--------------------------------------------------------------------------------

text :: Enum a => Bounded a => String -> VDOM a
text = view . Text

div :: Enum a => Bounded a => [Props a] -> [VDOM a] -> VDOM a
div props children = VDOM $ liftF $ View (Element "div" props children) id

--------------------------------------------------------------------------------

data A = One | Two deriving (Eq, Enum, Bounded)

test1 = do
  replicateM 3 $ do
    div [ onClick One ] [ text "1", text "11", text "111" ]
    div [ onClick Two ] [ text "2" ]
  pure ()

test2 x
  | x > 10 = pure ()
  | otherwise = do
      div [ onClick () ] [ text (show x) ]
      test2 (x + 1)

test3 = loop $ \recur -> do
  div [] [ div [ onClick () ] [ text "A" ], test1 ]
  div [] [ text "B", test2 0, test2 0 ]
  recur

sidebar = loop $ \recur -> do
  div [ onClick () ] [ text "BLACK" ] 
  div [ onClick () ] [ text "WHITE" ] 
  recur

test4 :: VDOM ()
test4 = div [] [ sidebar, test3 ]

--------------------------------------------------------------------------------

dec :: Δ (Int -> Int)
dec = undefined

inc :: Δ (Int -> Int)
inc = undefined

toString :: Δ a -> Δ String
toString = undefined

data Action = Inc | Dec deriving (Show, Enum, Bounded)

counter v = loop $ \recur -> do
  r <- div []
    [ div [ onClick Inc ] [ text "-" ]
    , div [] [ text "0" ]
    , div [ onClick Dec ] [ text "+" ]
    ]

  recur
