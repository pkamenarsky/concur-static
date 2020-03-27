module Main where

import Lib

main :: IO ()
main = writeFile "out.html" $ generateModule test8
