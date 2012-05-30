{-# LANGUAGE TemplateHaskell, ScopedTypeVariables #-}

module Main where

import Derive

data Vector = Vector Float Float
deriveReadShow ''Vector ["(@, @)"]

data Employee = Employee { name :: String, heigth :: Float, salary :: Integer, coords :: Vector }
deriveReadShow ''Employee ["Emp { Name: @, Height: @, Salary: $@; Coordinates: @ }"]

data Figure = Segment Vector Vector | Triangle Vector Vector Vector | Quadrange Vector Vector Vector Vector
deriveReadShow ''Figure ["-@, @-", "<@, @, @>", "[@, @, @, @]"]

data Boolean = True' | False'
deriveReadShow ''Boolean ["true", "false"]

data Tree = Leaf Integer | Branch Tree Tree
deriveReadShow ''Tree ["@", "<@|@>"]

data Expr = I Integer
          | Add Expr Expr
          | Mul Expr Expr
          deriving Show
deriveRead ''Expr ["@", "(@+@)", "(@*@)"]

infixr 5 :-:
data List a = Empty | a :-: (List a)
deriveReadShow ''List ["[]", "@ : @"]

data Either' a b = Left' a | Right' b
deriveReadShow ''Either' ["<- @", "-> @"]

data KeyValue a b = KeyValue a b
deriveReadShow ''KeyValue ["@ => @"]

main = do
  putStrLn "-- Vector"
  print $ Vector 11.7 33.3
  print $ (read "(7.1, 10)" :: Vector)
  putStrLn ""
  putStrLn "-- Employee"
  print $ Employee "Stephen Hawking" 1.7 1000 (Vector 44 55)
  print $ (read "Emp { Name: \"Albert Einstein\", Height: 1.6, Salary: $900; Coordinates: (9, 7) }" :: Employee)
  putStrLn ""
  putStrLn "-- Figure"
  print $ Triangle (Vector 5 6) (Vector 7 8) (Vector 8 9)
  print $ (read "-(45, 79), (34, 78)-" :: Figure)
  print $ (read "[(1, 2), (3, 4), (5, 6), (7, 8)]" :: Figure)
  putStrLn ""
  putStrLn "-- Boolean"
  print $ True'
  print $ (read "false" :: Boolean)
  putStrLn ""
  putStrLn "-- Tree (from Gentle Introduction to Haskell)"
  print $ Branch (Leaf 1) (Branch (Leaf 2) (Leaf 3))
  print $ (read "<<7|<4|5>>|9>" :: Tree)
  putStrLn ""
  putStrLn "-- Arithmetic"
  print $ (read "((2+2)*(4+7))" :: Expr)
  putStrLn ""
  putStrLn "-- Infix list with type parameter (from LYaHfGG)"
  print $ 4 :-: 7 :-: 9 :-: Empty
  print $ (read "(4, 5) : (5, 4) : (7, 8) : []" :: (List Vector))
  putStrLn ""
  putStrLn "-- Either"
  print $ (Left' "I'm on the left" :: Either' String Integer)
  print $ (read "-> \"I'm on the right\"" :: (Either' Integer String))
  putStrLn ""
  putStrLn "-- KeyValue"
  print $ (KeyValue "John" 5)
  print $ (read "\"Jacob\" => 7" :: (KeyValue String Integer))
