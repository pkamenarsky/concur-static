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

import Control.Monad (replicateM_)
import Control.Monad.Fix
import Control.Monad.Free
import qualified Control.Monad.Trans.State.Strict as ST

import Data.Char (toUpper)
import Data.String
import Data.Maybe (catMaybes)
import Data.List (intersperse)

import Prelude hiding (div)

import Debug.Trace

someFunc :: IO ()
someFunc = putStrLn "someFunc"

--------------------------------------------------------------------------------

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

data Name = Name String | Done Int deriving (Eq)

instance Show Name where
  show (Name name) = name

newName :: ST.State (Int, a) Name
newName = ST.state $ \(i, a) -> (Name $ "_" <> show i, (i + 1, a))

enumAll :: forall t a. Enum a => Bounded a => t a -> [a]
enumAll _ = [(minBound :: a)..maxBound]

generate :: Enum a => Bounded a => VDOM a -> ST.State (Int, [(Name, String)]) Name
generate (VDOM (Pure a)) = pure $ Done (fromEnum a)
generate (VDOM (Free (Call name))) = pure name
generate (VDOM (Free (Loop vdom next))) = mfix $ \name ->
  generate (vdom (VDOM $ liftF $ Call name))
generate (VDOM (Free (View (Text t) next))) = pure $ Name ("t('" <> t <> "')")
-- generate (VDOM (Free (View (Text t) next))) = do
--   name <- newName
-- 
--   ST.modify $ \(i, m) -> (i, (name, mkBody name):m)
--   pure name
-- 
--   where
--     mkBody name = mconcat $ intersperse "\n"
--       [ "function " <> show name <> "(_, parent, index) {"
--       , "  const e = document.createTextNode(" <> show t <> ");"
--       , "  parent.insertBefore(e, parent.childNodes[index]);"
--       , "}"
--       ]

generate (VDOM (Free (View dom@(Element e props children) next))) = do
  chNames <- traverse generate children
  nexts   <- traverse generate (map (VDOM . next) (enumAll dom))

  name    <- newName
  body    <- mkBody name nexts chNames

  ST.modify $ \(i, m) -> (i, (name, body):m)
  pure name

  where
    mkBody name nexts chNames = pure $ mconcat $ intersperse "\n"
      [ "function " <> show name <> "(k, parent, index) {"
      , "  const e = document.createElement('" <> e <> "');"
      , "  parent.insertBefore(e, parent.childNodes[index]);"
      , ""
      , "  function next(r) {"
      , "    parent.removeChild(e);"
      , "    switch (r) {"
      , mconcat $ intersperse "\n"
          [ mconcat
              [ "      case " <> show (fromEnum value) <> ": "
              , case nextName of
                  Done r        -> "k(" <> show r <> "); "
                  Name nextName -> nextName <> "(k, parent, index); "
              , "break;"
              ]
          | (value, nextName) <- zip (enumAll dom) nexts
          ]
      , "    }"
      , "  };"
      , ""
      , mconcat $ intersperse "\n"
          [ mconcat $ intersperse "\n"
              [ "  e.addEventListener('" <> event <> "', function () { next(" <> show (fromEnum value) <> ") });"
              ]
          | Event event value <- props
          ]
      , ""
      , mconcat $ intersperse "\n"
          [ "  " <> show chName <> "(next, e, " <> show index <> ");"
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
  ,"function t(t) {"
  , "  return function (_, parent, index) {"
  , "    const e = document.createTextNode(t);"
  , "    parent.insertBefore(e, parent.childNodes[index]);"
  , "  };"
  , "}"
  , "</script>"
  , "</html>"
  ]
  where
    (startName, (_, fns)) = ST.runState (generate vdom) (0, [])

--------------------------------------------------------------------------------

data Props a
  = Attr String String
  | Event String a

attr :: String -> String -> Props a
attr = undefined

onClick :: a -> Props a
onClick a = Event "click" a

--------------------------------------------------------------------------------

unit :: VDOM () -> VDOM ()
unit = id

text :: Enum a => Bounded a => String -> VDOM a
text = view . Text

div :: Enum a => Bounded a => [Props a] -> [VDOM a] -> VDOM a
div props children = VDOM $ liftF $ View (Element "div" props children) id

div' :: [Props ()] -> [VDOM ()] -> VDOM ()
div' = div

button :: Enum a => Bounded a => [Props a] -> [VDOM a] -> VDOM a
button props children = VDOM $ liftF $ View (Element "button" props children) id

--------------------------------------------------------------------------------

data A = One | Two deriving (Eq, Enum, Bounded)

test1 = do
  div [ onClick One ] [ text "666" ]
  div [ onClick Two ] [ text "777" ]
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

test5 = do
  div [ onClick () ] [ text "bla" ]
  div [ onClick () ] [ text "bla2" ]
  pure ()

test7 = loop $ \recur -> do
  r <- div []
    [ button [ onClick One ] [ text "Button A" ]
    , button [ onClick Two ] [ text "Button B" ]
    ]

  case r of
    One -> div [ onClick () ] [ text "You clicked A" ]
    Two -> div [ onClick () ] [ text "You clicked B" ]

  button [ onClick () ] [ text "Click for next try" ]

  recur

test8 = unit $ div [] (replicate 10 test7)

-- range :: forall t a. Enum a => Bounded a => t a -> Int
-- range _ = fromEnum (maxBound :: a) - fromEnum (minBound :: a)
-- 
-- instance (Bounded a, Enum a, Enum b) => Enum (Either a b) where
--   toEnum x
--     | x >= range (undefined :: [a]) = Right (toEnum (x - range (undefined :: [a])))
--     | otherwise = Left (toEnum x)
--   fromEnum (Left a) = fromEnum a
--   fromEnum (Right b) = fromEnum b + range (undefined :: [a]) -- TODO: Proxy
-- 
-- instance (Bounded a, Bounded b) => Bounded (Either a b) where
--   minBound = Left minBound
--   maxBound = Right maxBound
-- 
-- test6 = do
--   r <- div []
--     [ Left  <$> div [ onClick () ] [ text "bla" ]
--     , Right <$> div [ onClick () ] [ text "bla2" ]
--     ]
--   pure ()
