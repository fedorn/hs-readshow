{-# LANGUAGE TemplateHaskell, FlexibleInstances #-}

module Derive where

import Language.Haskell.TH
import Control.Monad
import Control.Applicative

data Format = D | L String deriving Show

parse :: String -> String -> [Format]
parse       "" ""    = []
parse       "" rest  = [L rest]
parse ('@':xs) ""    =  D : parse xs ""
parse ('@':xs) rest  =  L rest : D : parse xs ""
parse   (x:xs) rest  =  parse xs (rest++[x])

data T1 = T1

genQPE :: Int -> Q ([PatQ], [ExpQ])
genQPE n = do
    ids <- replicateM n (newName "x")
    return (map varP ids, map varE ids)

deriveShow :: Name -> String -> Q [Dec]
deriveShow t fstr = do
  TyConI (DataD _ _ _ [NormalC name fields] _) <- reify t
  
  (pats, vars) <- genQPE (length fields)

  let f _ [] = [| "" |]
      f vars (L s:xs) = [| s ++ $(f vars xs) |]
      f (v:vars) (D:xs) = [| show $v ++ $(f vars xs) |]      

  [d| instance Show $(conT t) where
        show = $(lamE [(conP name pats)] (f vars (parse fstr "")))|]

genPE :: Int -> Q ([Pat], [Exp])
genPE n = do
    ids <- replicateM n (newName "x")
    return (map VarP ids, map VarE ids)
    
split' :: String -> [(String, String)]
split' xs = map (\n -> splitAt n xs) [0..(length xs)]

deriveRead :: Name -> String -> Q [Dec]
deriveRead t fstr = do
    TyConI (DataD _ _ _ [NormalC name fields] _) <- reify t
    (pats, vars) <- genPE (length fields)
    let format = parse fstr ""
    let buildComp :: [Pat] -> [Format] -> Exp -> Q [Stmt]
        buildComp _ [] r = do
                      return [(NoBindS (TupE [foldl AppE (ConE name) vars, r]))]
        buildComp vars (L s:xs) r = do
                      rr <- newName "rr"
                      ((BindS (TupP [LitP (StringL s), VarP rr]) (AppE (VarE 'split') (r))) :) <$> (buildComp vars xs (VarE rr))
        buildComp (v:vars) (D:xs) r = do
                      rr <- newName "rr"
                      ((BindS (TupP [v, VarP rr]) (SigE (AppE (VarE 'reads) (r)) (AppT ListT (AppT (AppT (TupleT 2) (ConT ''Integer)) (ConT ''String))) )) :) <$> (buildComp vars xs (VarE rr))


    [d|instance Read $(conT t) where
          readsPrec _ = $(do s <- newName "s"
                             lamE [varP s] (CompE <$> (buildComp pats (parse fstr "") (VarE s) ) )) |]

-- instance Read T where
--   readsPrec _ s = [(A x1 x2 x3, r) |
--                    ("(", r1) <- lex s,
--                    (x1,  r2) <- reads r1,
--                    (",", r3) <- lex r2,
--                    (x2, r4) <- reads r3,
--                    (",", r5) <- lex r4,
--                    (x3, r6) <- reads r5,
--                    (")", r) <- lex r6]


deriveReadShow :: Name -> String -> Q [Dec]
deriveReadShow t fstr =
  liftM2 (++) (deriveRead t fstr) (deriveShow t fstr)