{-# LANGUAGE TemplateHaskell #-}

module Main where

import Derive

data T = A Integer Integer Integer
$(deriveShow ''T "(@, @, @)")
-- $(deriveRead ''T "(@, @, @)")

instance Read T where
  readsPrec _ s = [(A x1 x2 x3, r) | ("(", r1) <- lex s,
                                     (x1,  r2) <- reads r1 :: [(Integer, String)],
                                     (",", r3) <- lex r2,
                                     (x2, r4) <- reads r3 :: [(Integer, String)],
                                     (",", r5) <- lex r4,
                                     (x3, r6) <- reads r5 :: [(Integer, String)],
                                     (")", r) <- lex r6]

main = do print $ A 11 22 30
          print $ (read "(23,   43,  822)" :: T)