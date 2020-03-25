{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TupleSections              #-}

module Lib where

import Control.Applicative
import Control.Monad.Fix
import Control.Monad.Free
import qualified Control.Monad.Trans.State.Strict as ST

import Data.Char (toUpper)
import Data.Maybe (catMaybes)
import Data.List (intersperse)

import Prelude hiding (div)

someFunc :: IO ()
someFunc = putStrLn "someFunc"

--------------------------------------------------------------------------------

data Δ a = Empty

data DOM a
  = Text (Δ String)
  | ConstText String
  | Element String [Props a] [VDOM a]

data DOMF next
  = forall a. View (DOM (Δ a)) (Δ a -> next)
  | forall a. Recur (VDOM a -> VDOM a) (a -> next)

deriving instance Functor DOMF

newtype VDOM a = VDOM (Free DOMF a)
  deriving (Functor, Applicative, Monad)

recur :: (VDOM a -> VDOM a) -> VDOM a
recur f = VDOM $ liftF $ Recur f id

view :: DOM (Δ a) -> VDOM (Δ a)
view v = VDOM $ liftF $ View v id

orr :: [VDOM a] -> VDOM a
orr = undefined

--------------------------------------------------------------------------------

newtype Name = Name String

instance Show Name where
  show (Name name) = name

newName :: ST.State (Int, a) Name
newName = ST.state $ \(i, a) -> (Name $ "_" <> show i, (i + 1, a))

generate :: VDOM a -> ST.State (Int, [(Name, String)]) Name
generate (VDOM (Pure a)) = pure $ Name "end"
generate (VDOM (Free (View (ConstText t) next))) = do
  name <- newName

  ST.modify $ \(i, m) -> (i, (name, mkBody name):m)
  pure name

  where
    mkBody name = mconcat $ intersperse "\n"
      [ "function " <> show name <> "(_parent, _index) {"
      , "  const e = document.createTextNode(" <> show t <> ");"
      , "  return e;"
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
      [ "function " <> show name <> "(parent, index) {"
      , "  const e = document.createElement('" <> e <> "');"
      , ""
      , mconcat $ intersperse "\n"
          [ mconcat $ intersperse "\n"
              [ "  e.addEventListener('" <> event <> "', function() {"
              , "    parent.removeChild(e);"
              , "    parent.insertBefore(" <> show nextName <> "(parent, index), parent.childNodes[index]);"
              , "  });"
              ]
          | event <- events
          ]
      , ""
      , mconcat $ intersperse "\n"
          [ "  e.appendChild(" <> show chName <> "(e, " <> show index <> "));"
          | (index, chName) <- zip [0..] chNames
          ]
      , ""
      , "  return e;"
      , "}"
      ]
    events = [ event | Event event <- props ]

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
  , "document.body.appendChild(" <> show startName <> "(document.body, 0));"
  , ""
  , "function end(_parent, _index) {"
  , "  return document.createTextNode('END');"
  , "};"
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

test1 = do
  r <- div [ onClick (from 1) ] [ text' "1", text' "11", text' "111" ]
  r <- div [ onClick (from 2) ] [ text' "2" ]
  pure "done"

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
