{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}

module Concur.Static where

import Control.Monad (replicateM_)
import Control.Monad.Fix (mfix)
import Control.Monad.Free (Free (Pure, Free), liftF)
import qualified Control.Monad.Trans.State.Strict as ST

import Data.Char (toUpper)
import Data.Maybe (catMaybes)
import Data.List (intersperse)

import Prelude hiding (div)

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
      , "  // Attributes"
      , mconcat $ intersperse "\n"
          [ "  e.setAttribute('" <> key <> "', " <> value <> ");"
          | Attr key value' <- props
          , let value = case value' of
                  AString v   -> "'" <> v <> "'"
                  ANumber v   -> show v
                  ABool True  -> "true"
                  ABool False -> "false"
          ]
      , mconcat $ intersperse "\n"
          [ "  e.style['" <> key <> "'] = '" <> value <> "';"
          | Style pairs <- props
          , (key, value) <- pairs
          ]
      , ""
      , "  // Events"
      , mconcat $ intersperse "\n"
          [ "  e.addEventListener('" <> event <> "', function () { next(" <> show (fromEnum value) <> ") });"
          | Event event value <- props
          ]
      , ""
      , "  // Children"
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
  , js
  , "</script>"
  , "</html>"
  ]
  where
    js = mconcat $ intersperse "\n"
      [ mconcat $ intersperse "\n\n"
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
      ]

    (startName, (_, fns)) = ST.runState (generate vdom) (0, [])

--------------------------------------------------------------------------------

range :: forall a. Enum a => Bounded a => a -> Int
range _ = fromEnum (maxBound :: a) - fromEnum (minBound :: a)

offset :: forall a. Enum a => Bounded a => a -> Int
offset a = fromEnum (minBound :: a)

instance (Bounded a, Bounded b, Enum a, Enum b) => Enum (Either a b) where
  toEnum x
    | x >= range (undefined :: a) + 1 = Right $ toEnum (x + offset (undefined :: b) - range (undefined :: a) - 1)
    | otherwise = Left $ toEnum (x + offset (undefined :: a))
  fromEnum (Left a) = fromEnum a - offset a
  fromEnum (Right b) = range (undefined :: a) + 1 + fromEnum b - offset b

instance (Bounded a, Bounded b) => Bounded (Either a b) where
  minBound = Left minBound
  maxBound = Right maxBound

--------------------------------------------------------------------------------

data AValue = AString String | ANumber Float | ABool Bool

data Props a
  = Attr String AValue
  | Style [(String, String)]
  | Event String a
