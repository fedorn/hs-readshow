{-# LANGUAGE TemplateHaskell #-}

module Main where

import Derive

data Dot = Dot Integer Integer
deriveReadShow ''Dot ["(@, @)"]

data Circle = Circle { radius :: Integer, x :: Integer, y :: Integer }
deriveReadShow ''Circle ["Radius: @, X: @, Y: @"]

data Employee = Employee { name :: String, heigth :: Float, salary :: Integer, coords :: Dot, headShape :: Circle }
deriveReadShow ''Employee ["Emp { Name: @, Height: @, Salary: $@; Coordinates: @, Head: [@] }"]

data Figure = Segment Dot Dot | Triangle Dot Dot Dot | Quadrange Dot Dot Dot Dot
deriveShow ''Figure ["-@, @-", "<@, @, @>", "[@, @, @, @]"]

data List' = Cons' Integer List' | Nil'
deriveShow ''List' ["@:@", "[]"]

main = do print $ Dot 11 33
          print $ (read "(7, 10)" :: Dot)
          print $ Circle 2 1 3
          print $ (read "Radius: 5, X: 10, Y: 11" :: Circle)
          print $ Employee "Stephen Hawking" 1.7 1000 (Dot 44 55) (Circle 2 3 4)
          print $ (read "Emp { Name: \"Albert Einstein\", Height: 1.6, Salary: $900; Coordinates: (9, 7), Head: [Radius: 10, X: 8, Y: 44] }" :: Employee)
          print $ Segment (Dot 1 2) (Dot 3 4)
          print $ Triangle (Dot 5 6) (Dot 7 8) (Dot 8 9)
          print $ Quadrange (Dot 10 11) (Dot 12 13) (Dot 14 15) (Dot 16 17)
          print $ Cons' 4 (Cons' 7 (Cons' 9 Nil'))