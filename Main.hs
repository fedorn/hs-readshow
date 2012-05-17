{-# LANGUAGE TemplateHaskell #-}

module Main where

import Derive

data T = A Integer Integer Integer
deriveReadShow ''T "Radius:@, X:@, Y:@"

main = do print $ A 11 33 30
          print $ (read "Radius:5, X:1, Y:2" :: T)