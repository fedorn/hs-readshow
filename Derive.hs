{-# LANGUAGE TemplateHaskell, FlexibleInstances #-}

module Derive where

import Language.Haskell.TH
import Control.Monad

data Format = D | L String

-- Парсер строки форматирования – преобразовывает её в структуру Format
parse :: String -> String -> [Format]
parse       "" rest  = [L rest]
parse ('@':xs) rest  =  L rest : D : parse xs ""
parse   (x:xs) rest  =  parse xs (rest++[x])

data T1 = T1

genPE :: Int -> Q ([PatQ], [ExpQ])
genPE n = do
    ids <- replicateM n (newName "x")
    return (map varP ids, map varE ids)

showClause :: Con -> [Format] -> Q Clause
showClause (NormalC name fields) format = do
    let constructorName = nameBase name
    (pats,vars) <- genPE (length fields)
        
    let f [] [] = [| "" |]
        f [] [L s] = [| s |]
        f vars (L s:xs) = [| s ++ $(f vars xs) |]
        f (v:vars) (D:xs) = [| show $v ++ $(f vars xs) |]
                
    clause [conP name pats]
           (normalB [| $(f vars format) |]) []

deriveShow :: Name -> String -> Q [Dec]
deriveShow t fstr = do
    TyConI (DataD _ _ _ constructors _) <- reify t

    showbody <- showClause (head constructors) (parse fstr "")

    d <- [d| instance Show T1 where
                show x = "text"
          |]
    let    [InstanceD [] (AppT showt (ConT _T1)) [FunD showf _text]] = d
    return [InstanceD [] (AppT showt (ConT t  )) [FunD showf [showbody]]]

-- deriveRead :: Name -> String -> Q [Dec]
-- deriveRead name fstr = [d|instance Read $(conT name) where readsPrec _ _ = 1|]