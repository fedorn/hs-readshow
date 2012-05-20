{-# LANGUAGE TemplateHaskell, FlexibleInstances #-}

module Derive where

import Language.Haskell.TH
import Control.Monad
import Control.Applicative

data Format = D | L String deriving Show

parse :: String -> String -> [Format]
parse ""   ""       = []
parse rest ""       = [L rest]
parse ""   ('@':xs) =  D : parse "" xs
parse rest ('@':xs) =  L rest : D : parse "" xs
parse rest (x:xs)   =  parse (rest++[x]) xs

data T1 = T1

genQPE :: Int -> Q ([PatQ], [ExpQ])
genQPE n = do
    ids <- replicateM n (newName "x")
    return (map varP ids, map varE ids)

deriveShow :: Name -> [String] -> Q [Dec]
deriveShow t formatStrings = do
  TyConI (DataD _ _ _ constructors _) <- reify t
  
  let nameOfConstr constructor = case constructor of
        NormalC name _ -> name
        RecC    name _ -> name
        
  let nFieldsOfConstr constructor = case constructor of
        NormalC _ fields -> length fields
        RecC    _ fields -> length fields
        
  let f _ [] = [| "" |]
      f vars (L s:xs) = [| s ++ $(f vars xs) |]
      f (v:vars) (D:xs) = [| show $v ++ $(f vars xs) |]
      
  let showClause constructor format = do
        (pats, vars) <- genQPE (nFieldsOfConstr constructor)
        clause [conP (nameOfConstr constructor) pats]
          (normalB (f vars format)) []
          
  showbody <- zipWithM showClause constructors (map (parse "") formatStrings)

  return [InstanceD [] (AppT (ConT ''Show) (ConT t)) [FunD 'show showbody]]

genPE :: Int -> Q ([Pat], [Exp])
genPE n = do
    ids <- replicateM n (newName "x")
    return (map VarP ids, map VarE ids)
    
split' :: String -> [(String, String)]
split' xs = map (\n -> splitAt n xs) [0..(length xs)]

deriveRead :: Name -> String -> Q [Dec]
deriveRead t fstr = do
    TyConI (DataD _ _ _ constr _) <- reify t
    let (name, lenFields, types) = case constr of
          [NormalC name fields] -> (name, length fields, map (\(_, t) -> t) fields) 
          [RecC name fields] -> (name, length fields, map (\(_, _, t) -> t) fields)
    (pats, vars) <- genPE lenFields
    let buildComp :: [Type] -> [Pat] -> [Format] -> Exp -> Q [Stmt]
        buildComp _ _ [] r = do
                      return [(NoBindS (TupE [foldl AppE (ConE name) vars, r]))]
        buildComp types vars (L s:xs) r = do
                      rr <- newName "rr"
                      ((BindS (TupP [LitP (StringL s), VarP rr]) (AppE (VarE 'split') (r))) :) <$> (buildComp types vars xs (VarE rr))
        buildComp (t:types) (v:vars) (D:xs) r = do
                      rr <- newName "rr"
                      ((BindS (TupP [v, VarP rr]) (SigE (AppE (VarE 'reads) (r)) (AppT ListT (AppT (AppT (TupleT 2) t) (ConT ''String))) )) :) <$> (buildComp types vars xs (VarE rr))


    [d|instance Read $(conT t) where
          readsPrec _ = $(do s <- newName "s"
                             lamE [varP s] (CompE <$> (buildComp types pats (parse "" fstr) (VarE s) ) )) |]

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
  liftM2 (++) (deriveRead t (head formatStrings)) (deriveShow t formatStrings)