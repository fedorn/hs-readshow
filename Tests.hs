import ReadShow

data Letter = Letter Char
--            deriving Show

instance ReadShow Letter where
  toGrammar (Letter c) = Lit [c]
  fromGrammar (Lit (c:_)) = Letter c

data Abc = A | B | C | Pair Abc Abc

instance ReadShow Abc where
  toGrammar A = Lit "A"
  toGrammar B = Lit "B"
  toGrammar C = Lit "C"
  toGrammar (Pair one two) = Concat [Lit "(", toGrammar one, Lit ", ", toGrammar two, Lit ")"]
  
main = do
  print (Letter 'X')
  print (Pair A (Pair B C))