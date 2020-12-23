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
  | Element (Maybe String) String [Props a] [VDOM a]

data DOMF next
  = forall a. (Enum a, Bounded a) => View (DOM a) (a -> next)
  | forall a. (Enum a, Bounded a) => Loop (VDOM a -> VDOM a) (a -> next)
  | forall a. (Enum a, Bounded a) => Loop2
      (VDOM a -> VDOM a -> VDOM a)
      (VDOM a -> VDOM a -> VDOM a)
      (a -> next)
  | forall a. (Enum a, Bounded a) => Loop3
      (VDOM a -> VDOM a -> VDOM a -> VDOM a)
      (VDOM a -> VDOM a -> VDOM a -> VDOM a)
      (VDOM a -> VDOM a -> VDOM a -> VDOM a)
      (a -> next)
  | forall a. (Enum a, Bounded a) => Loop4
      (VDOM a -> VDOM a -> VDOM a -> VDOM a -> VDOM a)
      (VDOM a -> VDOM a -> VDOM a -> VDOM a -> VDOM a)
      (VDOM a -> VDOM a -> VDOM a -> VDOM a -> VDOM a)
      (VDOM a -> VDOM a -> VDOM a -> VDOM a -> VDOM a)
      (a -> next)
  | forall a. (Enum a, Bounded a) => Loop5
      (VDOM a -> VDOM a -> VDOM a -> VDOM a -> VDOM a -> VDOM a)
      (VDOM a -> VDOM a -> VDOM a -> VDOM a -> VDOM a -> VDOM a)
      (VDOM a -> VDOM a -> VDOM a -> VDOM a -> VDOM a -> VDOM a)
      (VDOM a -> VDOM a -> VDOM a -> VDOM a -> VDOM a -> VDOM a)
      (VDOM a -> VDOM a -> VDOM a -> VDOM a -> VDOM a -> VDOM a)
      (a -> next)
  | forall a. (Enum a, Bounded a) => Loop6
      (VDOM a -> VDOM a -> VDOM a -> VDOM a -> VDOM a -> VDOM a -> VDOM a)
      (VDOM a -> VDOM a -> VDOM a -> VDOM a -> VDOM a -> VDOM a -> VDOM a)
      (VDOM a -> VDOM a -> VDOM a -> VDOM a -> VDOM a -> VDOM a -> VDOM a)
      (VDOM a -> VDOM a -> VDOM a -> VDOM a -> VDOM a -> VDOM a -> VDOM a)
      (VDOM a -> VDOM a -> VDOM a -> VDOM a -> VDOM a -> VDOM a -> VDOM a)
      (VDOM a -> VDOM a -> VDOM a -> VDOM a -> VDOM a -> VDOM a -> VDOM a)
      (a -> next)
  | Call Name

deriving instance Functor DOMF

newtype VDOM a = VDOM (Free DOMF a)
  deriving (Functor, Applicative, Monad)

loop :: Enum a => Bounded a => (VDOM a -> VDOM a) -> VDOM a
loop f = VDOM $ liftF $ Loop f id

loop2 :: Enum a => Bounded a
  => (VDOM a -> VDOM a -> VDOM a)
  -> (VDOM a -> VDOM a -> VDOM a)
  -> VDOM a
loop2 f g = VDOM $ liftF $ Loop2 f g id

loop3 :: Enum a => Bounded a
  => (VDOM a -> VDOM a -> VDOM a -> VDOM a)
  -> (VDOM a -> VDOM a -> VDOM a -> VDOM a)
  -> (VDOM a -> VDOM a -> VDOM a -> VDOM a)
  -> VDOM a
loop3 f g h = VDOM $ liftF $ Loop3 f g h id

loop4 :: Enum a => Bounded a
  => (VDOM a -> VDOM a -> VDOM a -> VDOM a -> VDOM a)
  -> (VDOM a -> VDOM a -> VDOM a -> VDOM a -> VDOM a)
  -> (VDOM a -> VDOM a -> VDOM a -> VDOM a -> VDOM a)
  -> (VDOM a -> VDOM a -> VDOM a -> VDOM a -> VDOM a)
  -> VDOM a
loop4 f g h i = VDOM $ liftF $ Loop4 f g h i id

loop5 :: Enum a => Bounded a
  => (VDOM a -> VDOM a -> VDOM a -> VDOM a -> VDOM a -> VDOM a)
  -> (VDOM a -> VDOM a -> VDOM a -> VDOM a -> VDOM a -> VDOM a)
  -> (VDOM a -> VDOM a -> VDOM a -> VDOM a -> VDOM a -> VDOM a)
  -> (VDOM a -> VDOM a -> VDOM a -> VDOM a -> VDOM a -> VDOM a)
  -> (VDOM a -> VDOM a -> VDOM a -> VDOM a -> VDOM a -> VDOM a)
  -> VDOM a
loop5 f g h i j = VDOM $ liftF $ Loop5 f g h i j id

loop6 :: Enum a => Bounded a
  => (VDOM a -> VDOM a -> VDOM a -> VDOM a -> VDOM a -> VDOM a -> VDOM a)
  -> (VDOM a -> VDOM a -> VDOM a -> VDOM a -> VDOM a -> VDOM a -> VDOM a)
  -> (VDOM a -> VDOM a -> VDOM a -> VDOM a -> VDOM a -> VDOM a -> VDOM a)
  -> (VDOM a -> VDOM a -> VDOM a -> VDOM a -> VDOM a -> VDOM a -> VDOM a)
  -> (VDOM a -> VDOM a -> VDOM a -> VDOM a -> VDOM a -> VDOM a -> VDOM a)
  -> (VDOM a -> VDOM a -> VDOM a -> VDOM a -> VDOM a -> VDOM a -> VDOM a)
  -> VDOM a
loop6 f g h i j k = VDOM $ liftF $ Loop6 f g h i j k id

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
generate (VDOM (Free (Loop2 vdoma vdomb next))) = fmap fst $ mfix $ \(~(namea, nameb)) -> (,)
  <$> generate (vdoma (VDOM $ liftF $ Call namea) (VDOM $ liftF $ Call nameb)) 
  <*> generate (vdomb (VDOM $ liftF $ Call namea) (VDOM $ liftF $ Call nameb)) 
generate (VDOM (Free (Loop3 vdoma vdomb vdomc next))) = fmap f $ mfix $ \(~(namea, nameb, namec)) -> (,,)
  <$> generate (vdoma (VDOM $ liftF $ Call namea) (VDOM $ liftF $ Call nameb) (VDOM $ liftF $ Call namec)) 
  <*> generate (vdomb (VDOM $ liftF $ Call namea) (VDOM $ liftF $ Call nameb) (VDOM $ liftF $ Call namec)) 
  <*> generate (vdomc (VDOM $ liftF $ Call namea) (VDOM $ liftF $ Call nameb) (VDOM $ liftF $ Call namec)) 
  where
    f (a, _, _) = a
generate (VDOM (Free (Loop4 vdoma vdomb vdomc vdomd next))) = fmap f $ mfix $ \(~(namea, nameb, namec, named)) -> (,,,)
  <$> generate (vdoma (VDOM $ liftF $ Call namea) (VDOM $ liftF $ Call nameb) (VDOM $ liftF $ Call namec) (VDOM $ liftF $ Call named)) 
  <*> generate (vdomb (VDOM $ liftF $ Call namea) (VDOM $ liftF $ Call nameb) (VDOM $ liftF $ Call namec) (VDOM $ liftF $ Call named)) 
  <*> generate (vdomc (VDOM $ liftF $ Call namea) (VDOM $ liftF $ Call nameb) (VDOM $ liftF $ Call namec) (VDOM $ liftF $ Call named)) 
  <*> generate (vdomd (VDOM $ liftF $ Call namea) (VDOM $ liftF $ Call nameb) (VDOM $ liftF $ Call namec) (VDOM $ liftF $ Call named)) 
  where
    f (a, _, _, _) = a
generate (VDOM (Free (Loop5 vdoma vdomb vdomc vdomd vdome next))) = fmap f $ mfix $ \(~(namea, nameb, namec, named, namee)) -> (,,,,)
  <$> generate (vdoma (VDOM $ liftF $ Call namea) (VDOM $ liftF $ Call nameb) (VDOM $ liftF $ Call namec) (VDOM $ liftF $ Call named) (VDOM $ liftF $ Call namee)) 
  <*> generate (vdomb (VDOM $ liftF $ Call namea) (VDOM $ liftF $ Call nameb) (VDOM $ liftF $ Call namec) (VDOM $ liftF $ Call named) (VDOM $ liftF $ Call namee)) 
  <*> generate (vdomc (VDOM $ liftF $ Call namea) (VDOM $ liftF $ Call nameb) (VDOM $ liftF $ Call namec) (VDOM $ liftF $ Call named) (VDOM $ liftF $ Call namee)) 
  <*> generate (vdomd (VDOM $ liftF $ Call namea) (VDOM $ liftF $ Call nameb) (VDOM $ liftF $ Call namec) (VDOM $ liftF $ Call named) (VDOM $ liftF $ Call namee)) 
  <*> generate (vdome (VDOM $ liftF $ Call namea) (VDOM $ liftF $ Call nameb) (VDOM $ liftF $ Call namec) (VDOM $ liftF $ Call named) (VDOM $ liftF $ Call namee)) 
  where
    f (a, _, _, _, _) = a
generate (VDOM (Free (Loop6 vdoma vdomb vdomc vdomd vdome vdomf next))) = fmap f $ mfix $ \(~(namea, nameb, namec, named, namee, namef)) -> (,,,,,)
  <$> generate (vdoma (VDOM $ liftF $ Call namea) (VDOM $ liftF $ Call nameb) (VDOM $ liftF $ Call namec) (VDOM $ liftF $ Call named) (VDOM $ liftF $ Call namee) (VDOM $ liftF $ Call namef))
  <*> generate (vdomb (VDOM $ liftF $ Call namea) (VDOM $ liftF $ Call nameb) (VDOM $ liftF $ Call namec) (VDOM $ liftF $ Call named) (VDOM $ liftF $ Call namee) (VDOM $ liftF $ Call namef))
  <*> generate (vdomc (VDOM $ liftF $ Call namea) (VDOM $ liftF $ Call nameb) (VDOM $ liftF $ Call namec) (VDOM $ liftF $ Call named) (VDOM $ liftF $ Call namee) (VDOM $ liftF $ Call namef))
  <*> generate (vdomd (VDOM $ liftF $ Call namea) (VDOM $ liftF $ Call nameb) (VDOM $ liftF $ Call namec) (VDOM $ liftF $ Call named) (VDOM $ liftF $ Call namee) (VDOM $ liftF $ Call namef))
  <*> generate (vdome (VDOM $ liftF $ Call namea) (VDOM $ liftF $ Call nameb) (VDOM $ liftF $ Call namec) (VDOM $ liftF $ Call named) (VDOM $ liftF $ Call namee) (VDOM $ liftF $ Call namef))
  <*> generate (vdomf (VDOM $ liftF $ Call namea) (VDOM $ liftF $ Call nameb) (VDOM $ liftF $ Call namec) (VDOM $ liftF $ Call named) (VDOM $ liftF $ Call namee) (VDOM $ liftF $ Call namef))
  where
    f (a, _, _, _, _, _) = a
generate (VDOM (Free (View (Text t) next))) = pure $ Name ("t('" <> t <> "')")
generate (VDOM (Free (View dom@(Element ns e props children) next))) = do
  chNames <- traverse generate children
  nexts   <- traverse generate (map (VDOM . next) (enumAll dom))

  name    <- newName
  body    <- mkBody name nexts chNames

  ST.modify $ \(i, m) -> (i, (name, body):m)
  pure name

  where
    mkBody name nexts chNames = pure $ mconcat $ intersperse "\n"
      [ "function " <> show name <> "(k, parent, index) {"
      , case ns of
          Just ns' ->  "  const e = document.createElementNS('" <> ns' <> ", " <> e <> "');"
          Nothing  ->  "  const e = document.createElement('" <> e <> "');"
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
