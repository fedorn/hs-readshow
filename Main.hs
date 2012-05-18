{-# LANGUAGE TemplateHaskell #-}

module Main where

import Derive

data Vector = Vector Integer Integer
deriveReadShow ''Vector "(@, @)"

data Circle = Circle { radius :: Integer, x :: Integer, y :: Integer }
deriveReadShow ''Circle "Radius: @, X: @, Y: @"

main = do print $ Vector 11 33
          print $ (read "(7, 10)" :: Vector)
          print $ Circle 2 1 3
          print $ (read "Radius: 5, X: 10, Y: 11" :: Circle)