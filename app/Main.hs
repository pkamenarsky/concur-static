module Main where

import Concur.Static
import Concur.Static.DOM
import Concur.Static.DOM.Props
import Concur.Static.DOM.Events

import Prelude hiding (div)

data A = One | Two deriving (Show, Eq, Enum, Bounded)

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
  div [ style [("backgroundColor", "#777"), ("color", "fff")], onClick () ] [ text "BLACK" ] 
  div [ style [("backgroundColor", "#333"), ("color", "f00")], onClick () ] [ text "WHITE" ] 
  recur

test4 :: VDOM ()
test4 = div [] [ sidebar, test3 ]

test5 = do
  div [ onClick () ] [ text "bla" ]
  div [ onClick () ] [ text "bla2" ]
  pure ()

test7 = loop $ \recur -> do
  r <- div []
    [ Left  <$> button [ disabled True, onClick () ] [ text "Button A" ]
    , Right <$> button [ onClick () ] [ text "Button B" ]
    ]

  case r of
    Left _  -> div [ onClick () ] [ text "You clicked A!!!" ]
    Right _ -> div [ onClick () ] [ text "You clicked B!!!" ]

  button [ onClick () ] [ text "Click for next try" ]

  recur

test8 = unit $ div [] (replicate 10 test7)

data Menu = A | B | C
  deriving (Bounded, Enum)

site :: VDOM ()
site = loopMany [menuA, menuB, menuC]
  where
    menuA [recurA, recurB, recurC] = do
      choice <- div []
        [ div [] [ text "A" ]
        , div [ onClick B ] [ text "B" ]
        , div [ onClick C ] [ text "C" ]

        , do
            div [ onClick () ] [ text "This is the text of A" ]
            text "Done"
        ]

      case choice of
        A -> recurA
        B -> recurB
        C -> recurC

    menuB [recurA, recurB, recurC] = do
      choice <- div []
        [ div [ onClick A ] [ text "A" ]
        , div [] [ text "B" ]
        , div [ onClick C ] [ text "C" ]

        , do
            div [ onClick () ] [ text "This is the text of B" ]
            text "Done"
        ]

      case choice of
        A -> recurA
        B -> recurB
        C -> recurC

    menuC [recurA, recurB, recurC] = do
      choice <- div []
        [ div [ onClick A ] [ text "A" ]
        , div [ onClick B ] [ text "B" ]
        , div [] [ text "C" ]

        , do
            div [ onClick () ] [ text "This is the text of C" ]
            text "Done"
        ]

      case choice of
        A -> recurA
        B -> recurB
        C -> recurC

main :: IO ()
main = writeFile "out.html" $ generateModule site
