{-# LANGUAGE TemplateHaskell #-}

module Derive where

import Language.Haskell.TH
import Control.Monad
import Control.Applicative

-- Element of internal representation of format string.
data Format = D | L String deriving Show

-- Parse format string to its internal representation
parse :: String -> String -> [Format]
parse ""   ""             = []
parse rest ""             = [L rest]
parse rest ('\\':'@':xs)  = parse (rest++"@") xs  -- parse escaped @
parse ""   ('@':xs)       = D : parse "" xs
parse rest ('@':xs)       = L rest : D : parse "" xs
parse rest (x:xs)         = parse (rest++[x]) xs

-- Extract name from constructor
nameOfConstr :: Con -> Name
nameOfConstr constructor = case constructor of
  NormalC  name _ -> name
  RecC     name _ -> name
  InfixC _ name _ -> name

-- Extract number of fields from constructor
nFieldsOfConstr :: Con -> Int
nFieldsOfConstr constructor = case constructor of
  NormalC _ fields -> length fields
  RecC    _ fields -> length fields
  InfixC  _ _ _    -> 2

-- Extract types of fields from constructor
typeFieldsOfConstr :: Con -> [Type]
typeFieldsOfConstr constructor = case constructor of
  NormalC _ fields -> map (\(_, t) -> t) fields
  RecC    _ fields -> map (\(_, _, t) -> t) fields 
  InfixC x _ y -> map (\(_, t) -> t) [x, y]

-- Generate n unique variables and return them in form of patterns and expressions wrapped in Q monad
genQPE :: Int -> Q ([PatQ], [ExpQ])
genQPE n = do
    ids <- replicateM n (newName "x")
    return (map varP ids, map varE ids)

-- Return instance declaration of Show class for type t using formatStrings
deriveShow :: Name -> [String] -> Q [Dec]
deriveShow t formatStrings = do
  TyConI (DataD _ _ typeVars constructors _) <- reify t  -- extract type variables and constructors from t
  
  let typeVarsTypes = map (\(PlainTV name) -> VarT name) typeVars  -- make types for instance declaration
      
  -- Actiual function for showing
  let f _ [] = [| "" |]
      f vars (L s:xs) = [| s ++ $(f vars xs) |]  -- In case of L, just append it
      f (v:vars) (D:xs) = [| show $v ++ $(f vars xs) |]  -- In case of D, just append $ show it
      
  -- Generate function clause for one constructor
  let showClause constructor format = do
        (pats, vars) <- genQPE (nFieldsOfConstr constructor)  -- Get variables for left and right side of function definition
        clause [conP (nameOfConstr constructor) pats]
          (normalB (f vars format)) []
  
  -- Make body for function `show`
  showbody <- zipWithM showClause constructors (map (parse "") formatStrings)

  -- Return proper instance declaration, with context and t applied for its field types
  return [InstanceD (if null typeVars then [] else (map (ClassP ''Show) $ map (replicate 1) typeVarsTypes)) (AppT (ConT ''Show) (foldl AppT (ConT t) typeVarsTypes)) [FunD 'show showbody]]
                                                                                                                                                                      
-- Generate n unique variables and return them in form of patterns and expressions (not wrapped in Q monad now)
genPE :: Int -> Q ([Pat], [Exp])
genPE n = do
    ids <- replicateM n (newName "x")
    return (map VarP ids, map VarE ids)
    
-- Basic idea of generated parser:
-- instance Read T where
--   readsPrec _ s = [(A x1 x2 x3, r) |
--                    ("(", r1) <- split' s,
--                    (x1,  r2) <- reads r1,
--                    (",", r3) <- split' r2,
--                    (x2, r4) <- reads r3,
--                    (",", r5) <- split' r4,
--                    (x3, r6) <- reads r5,
--                    (")", r) <- split' r6] ++
--                   [(B x1 x2, r) |
--                    ...
    
-- Primitive readsPrec for String
split' :: String -> [(String, String)]
split' xs = map (\n -> splitAt n xs) [0..(length xs)]

-- Build statements of list comprehenstion
buildComp :: Con -> [Format] -> Exp -> Q [Stmt]
buildComp constructor pattern rest = do
  (pats, vars) <- genPE (nFieldsOfConstr constructor)  -- Get variables for left and right side of function definition
  
  -- Helper, uses extracted types and generated patterns
  let buildCompH :: [Type] -> [Pat] -> [Format] -> Exp -> Q [Stmt]
      
      -- Result expression of comprehension
      buildCompH _ _ [] rest = do
        return [(NoBindS (TupE [foldl AppE (ConE $ nameOfConstr constructor) vars, rest]))]
        
      -- Statement in case of L. In example it is `("(", r1) <- split' s,`
      buildCompH types pats (L s:xs) rest = do
        inRest <- newName "inRest"
        ((BindS (TupP [LitP (StringL s), VarP inRest]) (AppE (VarE 'split') (rest))) :) <$> (buildCompH types pats xs (VarE inRest))
        
      -- Statement in case of D. In example it is `(x1,  r2) <- reads r1,`
      -- Users should use ScopedTypeVariables GHC extension
      buildCompH (t:types) (p:pats) (D:xs) rest = do
        inRest <- newName "inRest"
        ((BindS (TupP [p, VarP inRest]) (SigE (AppE (VarE 'reads) (rest)) (AppT ListT (AppT (AppT (TupleT 2) t) (ConT ''String))) )) :) <$> (buildCompH types pats xs (VarE inRest))
                                                     
  buildCompH (typeFieldsOfConstr constructor) pats pattern rest
                                                     
-- Return instance declaration of Show class for type t using formatStrings
deriveRead :: Name -> [String] -> Q [Dec]
deriveRead t formatStrings = do
    TyConI (DataD _ _ typeVars constructors _) <- reify t  -- extract type variables and constructors from t
    
    let typeVarsTypes = map (\(PlainTV name) -> VarT name) typeVars  -- make types for instance declaration
    
    -- Generate argument for left and right side of readsPrec
    (sPatList, sExpList) <- genPE 1
    let sP = head sPatList
        sE = head sExpList
        
    -- Return proper instance declaration, with context and t applied for its field types. See basic idea above to understand readsPrec body.
    fmap (:[]) (instanceD (return (if null typeVars then [] else (map (ClassP ''Read) $ map (replicate 1) typeVarsTypes))) (return (AppT (ConT ''Read) (foldl AppT (ConT t) typeVarsTypes))) 
                      [funD 'readsPrec [clause [wildP, return sP]
                                               (normalB (appE [| foldl (++) [] |]
                                                              (listE (zipWith (\constructor formatString ->
                                                                                (CompE <$> (buildComp constructor (parse "" formatString) sE)))
                                                                              constructors formatStrings)))) []]])

-- Combine Show and Read instance declarations
deriveReadShow :: Name -> [String] -> Q [Dec]
deriveReadShow t formatStrings =
  liftM2 (++) (deriveShow t formatStrings) (deriveRead t formatStrings)