{-# LANGUAGE TemplateHaskell #-}

module Main where

import Derive

data Vector = Vector Integer Integer
deriveReadShow ''Vector "(@, @)"

data Circle = Circle { radius :: Integer, x :: Integer, y :: Integer }
deriveReadShow ''Circle "Radius: @, X: @, Y: @"

data Employee = Employee { name :: String, heigth :: Float, salary :: Integer, coords :: Vector, headShape :: Circle }
deriveReadShow ''Employee "Emp { Name: @, Height: @, Salary: $@; Coordinates: @, Head: [@] }"

main = do print $ Vector 11 33
          print $ (read "(7, 10)" :: Vector)
          print $ Circle 2 1 3
          print $ (read "Radius: 5, X: 10, Y: 11" :: Circle)
          print $ Employee "Stephen Hawking" 1.7 1000 (Vector 44 55) (Circle 2 3 4)
          print $ (read "Emp { Name: \"Albert Einstein\", Height: 1.6, Salary: $900; Coordinates: (9, 7), Head: [Radius: 10, X: 8, Y: 44] }" :: Employee) 