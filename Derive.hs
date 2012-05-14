{-# LANGUAGE TemplateHaskell #-}

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

-- Вспомогательная функция, которая генерирует 
-- n уникальных имён для образцов и выражений
genPE :: Int -> Q ([PatQ], [ExpQ])
genPE n = do
    ids <- replicateM n (newName "x")
    return (map varP ids, map varE ids)

-- Собираем один клоз функции show для данного конструктора:
--   show (A x1 x2) = "A "++show x1++" "++show x2
showClause :: Con -> [Format] -> Q Clause
showClause (NormalC name fields) format = do
    -- Имя конструктора, т.е. "A". nameBase возвращает имя без квалификации
    let constructorName = nameBase name
    -- Генерируем имена переменных для левой и правой части определения 
    (pats,vars) <- genPE (length fields)
    -- Рекурсивно строим выражение (" "++show x1++...++"") из списка переменных [x1, ...]
    -- let f []       = [| pat |]
    --     f (v:vars) = [| " " ++ show $v ++ $(f vars) |]
        
    let f [] [] = [| "" |]
        f [] [L s] = [| s |]
        f vars (L s:xs) = [| s ++ $(f vars xs) |]
        f (v:vars) (D:xs) = [| show $v ++ $(f vars xs) |]
                
    -- Собираем один клоз функции
    clause [conP name pats]       -- (A x1 x2)
           (normalB [| $(f vars format) |]) []  -- "A"++" "++show x1++" "++show x2

-- Основной шаблон, который генерирует объявление воплощения класса Show
deriveShow :: Name -> String -> Q [Dec]
deriveShow t fstr = do
    -- Получаем список конструкторов данных для типа t
    TyConI (DataD _ _ _ constructors _) <- reify t

    -- Теперь собираем все клозы в тело функции show:
    --   show (A x1 x2) = "A "++show x1++" "++show x2
    --   show (B x1)    = "B "++show x1
    --   show C         = "C"
    showbody <- showClause (head constructors) (parse fstr "")

    -- Генерируем шаблон объявления воплощения и потом заменяем в нём
    -- имя типа (T1) и тело функции (x = "text") нашим сгенерированным showbody
    d <- [d| instance Show T1 where
                show x = "text"
          |]
    let    [InstanceD [] (AppT showt (ConT _T1)) [FunD showf _text]] = d
    return [InstanceD [] (AppT showt (ConT t  )) [FunD showf [showbody]]]