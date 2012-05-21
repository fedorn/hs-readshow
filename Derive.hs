{-# LANGUAGE TemplateHaskell, FlexibleInstances #-}

module Derive where

import Language.Haskell.TH
import Control.Monad
import Control.Applicative

data Format = D | L String deriving Show

parse :: String -> String -> [Format]
parse ""   ""             = []
parse rest ""             = [L rest]
parse rest ('\\':'@':xs)  = parse (rest++"@") xs
parse ""   ('@':xs)       = D : parse "" xs
parse rest ('@':xs)       = L rest : D : parse "" xs
parse rest (x:xs)         = parse (rest++[x]) xs

nameOfConstr :: Con -> Name
nameOfConstr constructor = case constructor of
  NormalC name _ -> name
  RecC    name _ -> name
        
nFieldsOfConstr :: Con -> Int
nFieldsOfConstr constructor = case constructor of
  NormalC _ fields -> length fields
  RecC    _ fields -> length fields
  
typeFieldsOfConstr :: Con -> [Type]
typeFieldsOfConstr constructor = case constructor of
  NormalC _ fields -> map (\(_, t) -> t) fields
  RecC    _ fields -> map (\(_, _, t) -> t) fields 

genQPE :: Int -> Q ([PatQ], [ExpQ])
genQPE n = do
    ids <- replicateM n (newName "x")
    return (map varP ids, map varE ids)

deriveShow :: Name -> [String] -> Q [Dec]
deriveShow t formatStrings = do
  TyConI (DataD _ _ typeVars constructors _) <- reify t
  
  let typeVarsTypes = map (\(PlainTV name) -> VarT name) typeVars
        
  let f _ [] = [| "" |]
      f vars (L s:xs) = [| s ++ $(f vars xs) |]
      f (v:vars) (D:xs) = [| show $v ++ $(f vars xs) |]
      
  let showClause constructor format = do
        (pats, vars) <- genQPE (nFieldsOfConstr constructor)
        clause [conP (nameOfConstr constructor) pats]
          (normalB (f vars format)) []
          
  showbody <- zipWithM showClause constructors (map (parse "") formatStrings)

  return [InstanceD (if null typeVars then [] else [ClassP ''Show typeVarsTypes]) (AppT (ConT ''Show) (foldl AppT (ConT t) typeVarsTypes)) [FunD 'show showbody]]

genPE :: Int -> Q ([Pat], [Exp])
genPE n = do
    ids <- replicateM n (newName "x")
    return (map VarP ids, map VarE ids)
    
split' :: String -> [(String, String)]
split' xs = map (\n -> splitAt n xs) [0..(length xs)]

buildComp :: Con -> [Format] -> Exp -> Q [Stmt]
buildComp constructor pattern rest = do
  (pats, vars) <- genPE (nFieldsOfConstr constructor)
  let buildCompH :: [Type] -> [Pat] -> [Format] -> Exp -> Q [Stmt]
      buildCompH _ _ [] rest = do
        return [(NoBindS (TupE [foldl AppE (ConE $ nameOfConstr constructor) vars, rest]))]
      buildCompH types vars (L s:xs) rest = do
        inRest <- newName "inRest"
        ((BindS (TupP [LitP (StringL s), VarP inRest]) (AppE (VarE 'split') (rest))) :) <$> (buildCompH types vars xs (VarE inRest))
      buildCompH (t:types) (v:vars) (D:xs) rest = do
        inRest <- newName "inRest"
        ((BindS (TupP [v, VarP inRest]) (SigE (AppE (VarE 'reads) (rest)) (AppT ListT (AppT (AppT (TupleT 2) t) (ConT ''String))) )) :) <$> (buildCompH types vars xs (VarE inRest))
  buildCompH (typeFieldsOfConstr constructor) pats pattern rest

deriveRead :: Name -> [String] -> Q [Dec]
deriveRead t formatStrings = do
    TyConI (DataD _ _ typeVars constructors _) <- reify t
    
    let typeVarsTypes = map (\(PlainTV name) -> VarT name) typeVars
    
    (sPatList, sExpList) <- genPE 1
    let sP = head sPatList
        sE = head sExpList
    fmap (:[]) (instanceD (return (if null typeVars then [] else [ClassP ''Read typeVarsTypes])) (return (AppT (ConT ''Read) (foldl AppT (ConT t) typeVarsTypes))) 
                      [funD 'readsPrec [clause [wildP, return sP]
                                               (normalB (appE [| foldl (++) [] |]
                                                              (listE (zipWith (\constructor formatString ->
                                                                                (CompE <$> (buildComp constructor (parse "" formatString) sE)))
                                                                              constructors formatStrings)))) []]])

-- instance Read T where
--   readsPrec _ s = [(A x1 x2 x3, r) |
--                    ("(", r1) <- lex s,
--                    (x1,  r2) <- reads r1,
--                    (",", r3) <- lex r2,
--                    (x2, r4) <- reads r3,
--                    (",", r5) <- lex r4,
--                    (x3, r6) <- reads r5,
--                    (")", r) <- lex r6]


deriveReadShow :: Name -> [String] -> Q [Dec]
deriveReadShow t formatStrings =
  liftM2 (++) (deriveShow t formatStrings) (deriveRead t formatStrings)