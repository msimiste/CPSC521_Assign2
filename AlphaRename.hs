
module AlphaRename where

import ParseProg
import AST
import Data.List
import Data.Maybe


type AItem = (String, String)
type AList = ([AItem],Int)
type ST = ([AItem],[AItem], Int, Int)


alphaRename::  (Prog String String) -> (ST, (Prog String String))
alphaRename (Prog prog) = (table, (Prog functions)) where 
            (table, functions) = goFun (([],[],0,0),funcs)
            funcs = funList (Prog prog)

funList:: (Prog String String) -> [Fun String String]
funList (Prog funcs) = funcs

goFun:: (ST, [Fun String String]) ->(ST, [Fun String String])
goFun (table, []) = (table,[])
goFun (table, (f:fs)) = (tbl, fcns) where
    (st1, fcn) = renameFunction (table, f)
    (st2, fss) = goFun (st1,fs)
    (tbl,fcns) = (st2,fcn:fss)

  
renameFunction:: (ST, (Fun String String)) -> (ST, Fun String String)
renameFunction (table, fun) = (st, function) where
    (st1, fun1) = replaceFuncName (table, fun)
    (st2, fun2) = replaceFuncArgs (st1, fun1)
    (st, function) = replaceFuncExp (st2, fun2)

replaceFuncName:: (ST,(Fun String String)) -> (ST,(Fun String String))
replaceFuncName ((vList,fList,vNum,fNum),(Fun (name, args,exp))) = case (checkInList name fList) of 
    True -> ((vList, fList, vNum, fNum),retFunction) where
        (name, alias) = getAItem name fList        
        retFunction = (Fun (alias,args,exp)) 
    False -> ((vList,(name,alias):fList, vNum, (fNum+1)),retFunction) where
        alias = ("f" ++ show fNum)
        retFunction = (Fun (alias,args,exp))
        
replaceFuncArgs:: (ST,(Fun String String)) -> (ST, (Fun String String))
replaceFuncArgs (symTable,(Fun (name,args,exp))) = (table, (Fun (name, arguments, exp))) where
    (table, arguments) = replaceFuncArg symTable args


replaceFuncArg:: ST -> [String] -> (ST, [String])
replaceFuncArg tble [] = (tble, [])
replaceFuncArg (vList,fList,vNum,fNum) (arg:args) = (symTable, arguments) where
    (t1,a) = case (checkInList arg vList) of
        True -> ((vList,fList,vNum,fNum), alias) where
                (name,alias) = getAItem arg vList        
        False -> (((arg,alias):vList, fList,(vNum+1), fNum), alias) where
            alias = "X" ++ show vNum
    (t2,args1) = replaceFuncArg t1 args
    (symTable,arguments) = (t2,a:args1)

replaceFuncExp:: (ST,Fun String String) -> (ST,Fun String String)
replaceFuncExp (table, (Fun (n,a,exp))) = (tb1, (Fun (n,a,expression))) where
    (tb1, expression) = replaceExpName (table,exp)
      
replaceExpName:: (ST, (Exp String String)) -> (ST,(Exp String String))
replaceExpName ((vList, fList, vNum, fNum), exp) = case exp of
    (VAR exp1) -> case (checkInList exp1 vList) of 
        True -> ((vList,fList,vNum,fNum), VAR alias) where
            (name,alias) = getAItem exp1 vList
        False -> error $ "No Argument for variable: VAR " ++ exp1
        
    ADD exp1 exp2 -> (tb2, ADD express1 express2) where
        (tb1,express1) = replaceExpName ((vList, fList, vNum, fNum), exp1)
        (tb2, express2) = replaceExpName (tb1, exp2)
      
    SUB exp1 exp2 -> (tb2, SUB express1 express2) where
        (tb1,express1) = replaceExpName ((vList, fList, vNum, fNum), exp1)
        (tb2, express2) = replaceExpName (tb1, exp2)
         
    MUL exp1 exp2 -> (tb2, MUL express1 express2) where
        (tb1,express1) = replaceExpName ((vList, fList, vNum, fNum), exp1)
        (tb2, express2) = replaceExpName (tb1, exp2)
            
    DIV exp1 exp2 -> (tb2, DIV express1 express2) where
        (tb1,express1) = replaceExpName ((vList, fList, vNum, fNum), exp1)
        (tb2, express2) = replaceExpName (tb1, exp2)
        
    NEG exp1 -> (tb1, NEG express1) where
        (tb1, express1)  = replaceExpName ((vList, fList, vNum, fNum), exp1)
        
    CONST exp1 ->((vList,fList,vNum,fNum), CONST exp1) 
         
    COND bexp1 exp1 exp2 -> (tb3, COND bexpress1 express1 express2) where
        (tb1, bexpress1) = replaceBexpName ((vList, fList, vNum, fNum), bexp1)
        (tb2, express1) = replaceExpName (tb1, exp1)
        (tb3, express2) = replaceExpName (tb2, exp2)
            
    LET funcs exp2 -> (tb3, LET functions expression) where
        (tb1, functions1) = letFunctionP1 ((vList, fList, vNum,fNum), funcs)
        (tb2, functions) = renameLetFunctions (tb1, functions1)
        (tb3, expression) = replaceExpName (tb2, exp2)     
             
    APP exp1 exps -> (tb2, APP expression1 expressions) where
        (tb1, expression1) = replaceAppString ((vList, fList, vNum, fNum), exp1)
        (tb2, expressions) = replaceListOfExpressions (tb1, exps)
    
    
replaceBexpName:: (ST, (BExp String String)) -> (ST, (BExp String String))
replaceBexpName (table, exp) = case exp of

    Lt exp1 exp2  -> (tb2, Lt expression1 expression2) where
        (tb1, expression1) = replaceExpName (table, exp1)
        (tb2, expression2) = replaceExpName (tb1, exp2)
        
    Gt exp1 exp2  -> (tb2, Gt expression1 expression2) where
        (tb1, expression1) = replaceExpName (table, exp1)
        (tb2, expression2) = replaceExpName (tb1, exp2)
        
    Eq exp1 exp2  -> (tb2, Eq expression1 expression2) where
        (tb1, expression1) = replaceExpName (table, exp1)
        (tb2, expression2) = replaceExpName (tb1, exp2)
        
    AND bexp1 bexp2 -> (tb2, AND bexpression1 bexpression2) where
        (tb1, bexpression1) = replaceBexpName (table, bexp1)
        (tb2, bexpression2) = replaceBexpName (tb1, bexp2)
        
    OR bexp1 bexp2 -> (tb2, OR bexpression1 bexpression2) where
        (tb1, bexpression1) = replaceBexpName (table, bexp1)
        (tb2, bexpression2) = replaceBexpName (tb1, bexp2)
        
    NOT bexp1      -> (tb1, NOT bexpression1) where
        (tb1, bexpression1) = replaceBexpName (table, bexp1)
        
    TRUE  -> (table, TRUE)    
    FALSE -> (table, FALSE)
    
replaceAppString:: (ST, String) -> (ST,(String))
replaceAppString ((vList, fList, vNum, fNum), str) = case (checkInList str fList) of 
    True -> ((vList,fList,vNum,fNum), alias) where
        (name, alias) = getAItem str fList
    False ->  error $ "Function is not found: " ++ str 
    
letFunctionP1:: (ST, [Fun String String]) -> (ST,[Fun String String])
letFunctionP1 (table,[]) = (table,[])
letFunctionP1 (table, (f:fs)) = (symTab, functions) where
      (t1,fun1) = replaceFuncName (table,f)
      (t2,funs2) = letFunctionP1 (t1,fs)
      (symTab,functions) = (t2,((fun1):funs2))
   
renameLetFunctions:: (ST, [Fun String String]) -> (ST, [Fun String String])
renameLetFunctions (table, []) = (table, [])
renameLetFunctions (table, (l:list)) = (tb2, (fun1:funcs)) where
    (tb1, fun1) = renameLetFunction (table, l)
    (tb2, funcs) = renameLetFunctions (tb1,list)

renameLetFunction:: (ST, Fun String String) -> (ST,(Fun String String))
renameLetFunction  (table, fun) = (st, fun2) where    
    (st1, fun1) = replaceFuncArgs (table, fun)
    (st, fun2) = replaceFuncExp (st1,fun1)
    
--helpers to query and pull AItems from lists   
checkInList:: String -> [AItem] -> Bool
checkInList item list = (item `elem` (map fst list))
 
getIndex:: String -> [AItem] -> Int
getIndex item list = fromJust (elemIndex item (map fst list))

getAItem:: String -> [AItem] -> AItem
getAItem name list =  (name,alias) where
    index = getIndex name list
    (str, alias) = list !! index
 
replaceListOfExpressions:: (ST, [Exp String String]) -> (ST, [Exp String String])
replaceListOfExpressions (symTab, []) = (symTab,[])
replaceListOfExpressions (symTab, (e:exps)) = (symTable,expressions) where
    (st1, exp1) = replaceExpName (symTab, e)
    (st2, exprs) = replaceListOfExpressions (st1,exps)
    (symTable, expressions) = (st2,((exp1):exprs))


