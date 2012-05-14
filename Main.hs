{-# LANGUAGE TemplateHaskell #-}

module Main where

import Derive

data T = A Integer Integer Integer
data T1 = B Integer Integer Integer deriving Show
$(deriveShow ''T "(@, @, @)")

main = (print $ A 11 22 30) >> (print $ B 11 22 30)