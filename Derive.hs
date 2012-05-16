{-# LANGUAGE TemplateHaskell, FlexibleInstances #-}

module Derive where

import Language.Haskell.TH
import Control.Monad

data Format = D | L String

parse :: String -> String -> [Format]
parse       "" rest  = [L rest]
parse ('@':xs) rest  =  L rest : D : parse xs ""
parse   (x:xs) rest  =  parse xs (rest++[x])

data T1 = T1

genPE :: Int -> Q ([PatQ], [ExpQ])
genPE n = do
    ids <- replicateM n (newName "x")
    return (map varP ids, map varE ids)

deriveShow :: Name -> String -> Q [Dec]
deriveShow t fstr = do
  TyConI (DataD _ _ _ [NormalC name fields] _) <- reify t
  
  (pats, vars) <- genPE (length fields)

  let f [] [] = [| "" |]
      f [] [L s] = [| s |]
      f vars (L s:xs) = [| s ++ $(f vars xs) |]
      f (v:vars) (D:xs) = [| show $v ++ $(f vars xs) |]      

  [d| instance Show $(conT t) where
        show = $(lamE [(conP name pats)] (f vars (parse fstr "")))|]

-- readClause :: Con

-- deriveRead :: Name -> String -> Q [Dec]
-- deriveRead name fstr = [d|instance Read $(conT name) where readsPrec _ s = [|]