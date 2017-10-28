
module AlphaRename where

import ParseProg
import AST
import Data.List
import Data.Maybe




type AItem = (String, String)
type AList = ([AItem],Int)
type ST = ([AItem],[AItem], Int, Int)


alphaRename::  (Prog String String) -> (Prog String String)
alphaRename (Prog prog) = Prog functions where 
            (table, functions) = goFun (([],[],1,1),funcs)
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
replaceFuncExp (table, (Fun (n,a,exp))) = (table, (Fun (n,a,expression))) where
    expression = replaceExpName (table,exp)
      
replaceExpName:: (ST, (Exp String String)) -> (Exp String String)
replaceExpName ((vList, fList, vNum, fNum), exp) = case exp of
    (VAR exp1) -> case (checkInList exp1 vList) of 
        True -> VAR alias where
            (name,alias) = getAItem exp1 vList
        False -> error $ "No Argument for variable: VAR " ++ exp1
        
    ADD exp1 exp2 -> ADD express1 express2 where
        express1 = replaceExpName ((vList, fList, vNum, fNum), exp1)
        express2 = replaceExpName ((vList, fList, vNum, fNum), exp2)
      
    SUB exp1 exp2 -> SUB express1 express2 where 
        express1 = replaceExpName ((vList, fList, vNum, fNum), exp1)
        express2 = replaceExpName ((vList, fList, vNum, fNum), exp2)
         
    MUL exp1 exp2 -> MUL express1 express2 where 
        express1 = replaceExpName ((vList, fList, vNum, fNum), exp1)
        express2 = replaceExpName ((vList, fList, vNum, fNum), exp2)
        
    DIV exp1 exp2 -> DIV express1 express2 where 
        express1 = replaceExpName ((vList, fList, vNum, fNum), exp1)
        express2 = replaceExpName ((vList, fList, vNum, fNum), exp2)
        
    NEG exp1 -> NEG express1 where
        express1 = replaceExpName ((vList, fList, vNum, fNum), exp1)
        
    CONST exp1 -> CONST exp1 
         
    COND bexp1 exp1 exp2 -> COND bexpress1 express1 express2 where
        bexpress1 = replaceBexpName ((vList, fList, vNum, fNum), bexp1)
        express1 = replaceExpName ((vList, fList, vNum, fNum), exp1)
        express2 = replaceExpName ((vList, fList, vNum, fNum), exp2)
            
    LET funcs exp2 -> LET functions expression where
        (t1, funcs1) = letFunctionP1 ((vList, fList, vNum, fNum), funcs)
        functions = map (\func ->  renameLetFunction (t1, func)) funcs1        
        expression = replaceExpName (t1, exp2)        
      
    APP exp1 exps -> APP expression1 expressions where
        expression1 = replaceAppString ((vList, fList, vNum, fNum), exp1)
        expressions = map (\expr -> replaceExpName ((vList, fList, vNum, fNum), expr)) exps
    
    
replaceBexpName:: (ST, (BExp String String)) -> (BExp String String)
replaceBexpName (table, exp) = case exp of

    Lt exp1 exp2  -> Lt expression1 expression2 where
        expression1 = replaceExpName (table, exp1)
        expression2 = replaceExpName (table, exp2)
        
    Gt exp1 exp2  -> Gt expression1 expression2 where
        expression1 = replaceExpName (table, exp1)
        expression2 = replaceExpName (table, exp2)
        
    Eq exp1 exp2  -> Eq expression1 expression2 where
        expression1 = replaceExpName (table, exp1)
        expression2 = replaceExpName (table, exp2)
        
    AND exp1 exp2 -> AND bexpression1 bexpression2 where
        bexpression1 = replaceBexpName (table, exp1)
        bexpression2 = replaceBexpName (table, exp2)
        
    OR exp1 exp2  -> OR bexpression1 bexpression2 where
        bexpression1 = replaceBexpName (table, exp1)
        bexpression2 = replaceBexpName (table, exp2)
        
    NOT exp1      -> NOT bexpression1 where
        bexpression1 = replaceBexpName (table, exp1)
        
    TRUE          -> TRUE    
    FALSE         -> FALSE
    
replaceAppString:: (ST, String) -> (String)
replaceAppString ((vList, fList, vNum, fNum), str) = case (checkInList str fList) of 
    True -> alias where
        (name, alias) = getAItem str fList
    False -> error $ "Function is not found: " ++ str
    
letFunctionP1:: (ST, [Fun String String]) -> (ST,[Fun String String])
letFunctionP1 (table,[]) = (table,[])
letFunctionP1 (table, (f:fs)) = (symTab, functions) where
    (t1, fun1) = replaceFuncName (table, f)
    (t2, funs) = letFunctionP1 (t1,fs)
    (symTab, functions) = (t2, fun1:funs)

renameLetFunction:: (ST, Fun String String) -> (Fun String String)
renameLetFunction  (table, fun) = fun2 where
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
 
 
fun1 = (Prog [Fun ("main",["x","y","a","b"],(COND (Lt (VAR "a") (VAR "b")) (VAR "x") (VAR "y") ))])

fun2 = (Prog 
             [Fun ("main",["x","y"],(ADD (VAR "x") (VAR "y")))
             ,Fun ("f",["x"], (LET 
                   [Fun ("g",["y"],ADD (VAR "y") (VAR "x"))
                   ,Fun ("h",["x","y"], ADD (APP "main" [VAR "x"])(VAR "y"))]
                     (ADD (APP "g" [VAR "x"])
                     (APP "h" [VAR "x"])) ))])
                     
      
test3 = (Prog [Fun ("main",["x","y"],(ADD (VAR "x") (VAR "y")))
             ,Fun ("f",["z"], (LET 
                   [Fun ("g",["a"],MUL (VAR "z") (VAR "a"))
                   ,Fun ("h",["d","e"], DIV (VAR "d") (VAR "e"))]
                     (ADD (APP "h" [VAR "z"])
                     (APP "h" [VAR "z",CONST 7])) ))])
test5 = (Prog 
             [Fun ("main",["x","y"],(ADD (VAR "x") (VAR "y")))
             ,Fun ("f",["x"], (LET 
                   [Fun ("g",["y"],MUL (VAR "y") (VAR "x"))
                   ,Fun ("h",["x","y"], DIV (VAR "x") (VAR "y"))]
                     (ADD (APP "g" [VAR "x"])
                     (APP "h" [VAR "x",CONST 7])) ))])

testTable = ([("y","X1"),("x","X0")],[("main","X9")],2,1)::ST
