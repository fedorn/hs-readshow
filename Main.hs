{-# LANGUAGE TemplateHaskell #-}

module Main where

import Derive

data T = A Integer Integer Integer
deriveReadShow ''T "[@,@,@]"

main = do print $ A 11 33 30
          print $ (read "[44,55,22]" :: T)