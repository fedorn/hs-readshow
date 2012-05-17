{-# LANGUAGE TemplateHaskell #-}

module Main where

import Derive

data T = A Integer Integer Integer
deriveShow ''T "(@,@,@)"
deriveRead ''T "(@,@,@)"

-- instance Read T where
--   readsPrec _ s = [(A x1 x2 x3, r) |
--                    ("(", r1) <- lex s,
--                    (x1,  r2) <- reads r1,
--                    (",", r3) <- lex r2,
--                    (x2, r4) <- reads r3,
--                    (",", r5) <- lex r4,
--                    (x3, r6) <- reads r5,
--                    (")", r) <- lex r6]

main = do print $ A 11 33 30
          print $ (read "(44,55,22)" :: T)