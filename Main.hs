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
deriveReadShow ''Figure ["-@, @-", "<@, @, @>", "[@, @, @, @]"]

data Boolean = True' | False'
deriveReadShow ''Boolean ["true", "false"]

data List' = Cons' Integer List' | Nil'
deriveReadShow ''List' ["@ \\@ @", "nil"]

data Tree = Leaf Integer | Branch Tree Tree
deriveReadShow ''Tree ["@", "<@|@>"]

data List'' a = Cons'' a (List'' a) | Nil''
deriveShow ''List'' ["@:@", "[]"]

--data Just'' a = Just'' a
--instance Read a => Read (Just'' a) where
--  readsPrec _ s = [(q, w) <- reads s :: [(a, String)]]

main = do
  putStrLn "-- Dot"
  print $ Dot 11 33
  print $ (read "(7, 10)" :: Dot)
  putStrLn "-- Circle"
  print $ Circle 2 1 3
  print $ (read "Radius: 5, X: 10, Y: 11" :: Circle)
  putStrLn "-- Employee"
  print $ Employee "Stephen Hawking" 1.7 1000 (Dot 44 55) (Circle 2 3 4)
  print $ (read "Emp { Name: \"Albert Einstein\", Height: 1.6, Salary: $900; Coordinates: (9, 7), Head: [Radius: 10, X: 8, Y: 44] }" :: Employee)
  putStrLn "-- Figure"
  print $ Segment (Dot 1 2) (Dot 3 4)
  print $ Triangle (Dot 5 6) (Dot 7 8) (Dot 8 9)
  print $ Quadrange (Dot 10 11) (Dot 12 13) (Dot 14 15) (Dot 16 17)
  print $ (read "-(45, 79), (34, 78)-" :: Figure)
  print $ (read "<(1, 2), (3, 4), (5, 6)>" :: Figure)
  print $ (read "[(1, 2), (3, 4), (5, 6), (7, 8)]" :: Figure)
  putStrLn "-- Boolean"
  print $ True'
  print $ (read "false" :: Boolean)
  putStrLn "-- List'"
  print $ Cons' 4 (Cons' 7 (Cons' 9 Nil'))
  print $ (read "nil" :: List')
  print $ (read "7 @ 6 @ 45 @ nil" :: List')
  putStrLn "-- Tree (from Gentle Introduction to Haskell)"
  print $ Branch (Leaf 1) (Branch (Leaf 2) (Leaf 3))
  print $ (read "<<7|<4|5>>|9>" :: Tree)
  putStrLn "-- List with type parameter"
  print $ Cons'' 4 (Cons'' 7 (Cons'' 9 Nil''))
--  print $ (read "7:3:23:[]" :: (List'' Integer))