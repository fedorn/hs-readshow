{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module ReadShow where

data RSGrammar = Concat [RSGrammar]
--               | Kleene RSGrammar
               | Lit String
--               deriving Show

showGrammar :: RSGrammar -> String
showGrammar (Lit s) = s
showGrammar (Concat []) = ""
showGrammar (Concat (x:xs)) = (showGrammar x ++ showGrammar (Concat xs))

class ReadShow a where
  toGrammar :: a -> RSGrammar
  fromGrammar :: RSGrammar -> a
  
instance (ReadShow rs) => (Show rs) where
  show rs = showGrammar (toGrammar rs)
