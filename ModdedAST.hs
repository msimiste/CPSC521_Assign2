module ModdedAST where

import CallGraph
import AlphaRename
import LambdaLift
import AST
import Data.List
import Data.Maybe
import TestFiles
import ParseProg




modAST:: (Prog String String) ->  (Prog String String)
modAST prog =  Prog outFunctions where
    table = lambdaLift prog
    (functions, callG) = produceCallGraph prog
    outFunctions = modListOfFuncs (table,functions,[])
    
modListOfFuncs:: (Table, [Fun String String], [Fun String String]) -> [Fun String String]
modListOfFuncs (tableT,[], list) = list
modListOfFuncs (tableT, (f:funcs), list) = functions where
    funcs1 = modFunc (tableT, f, list)
    funcs2 = modListOfFuncs (tableT, funcs, funcs1)
    functions = funcs2   

modLetFuncs:: (Table, [Fun String String]) -> [Fun String String]
modLetFuncs (table, []) = []
modLetFuncs (table, (f:fs)) = funcs where
        fun = modFunc (table, f, [])
        (funs) = modLetFuncs (table, fs)
        funcs = (fun ++ funs)
    
modFunc:: (Table, (Fun String String), [Fun String String]) -> [Fun String String]
modFunc (tableT, (Fun (name,args,exp)), list) = ((Fun (name,arguments,expression)):functions) where
    (expression, functions) = modFuncExpression (tableT,exp)
    arguments = getFFArgs (fst tableT) name
 
modFuncExpression:: (Table, (Exp String String)) -> ((Exp String String), [Fun String String])
modFuncExpression (table, expr) = case expr of
    
    LET funs exp1 -> (expression,(f ++ functions1)) where
            (functions1) = modLetFuncs (table, funs)
            (expression, f) = modFuncExpression (table, exp1)
   
    APP n exps -> ((APP n exprs) ,[]) where
            addedList = addExpressions (table,n)        
            exprs = (exps ++ addedList)
            
    VAR exp1 -> (VAR exp1, [])  
        
    ADD exp1 exp2 -> ((ADD expression1 expression2), list) where
            (expression1, l1) = modFuncExpression (table, exp1)
            (expression2, l2) = modFuncExpression (table, exp2)
            list = l1 ++ l2
       
    SUB exp1 exp2 -> ((SUB expression1 expression2), list) where
            (expression1, l1) = modFuncExpression (table, exp1)
            (expression2, l2) = modFuncExpression (table, exp2)
            list = l1 ++ l2
        
    MUL exp1 exp2 -> ((MUL expression1 expression2), list) where
            (expression1, l1) = modFuncExpression (table, exp1)
            (expression2, l2) = modFuncExpression (table, exp2)
            list = l1 ++ l2
        
    DIV exp1 exp2 -> ((DIV expression1 expression2), list) where
            (expression1, l1) = modFuncExpression (table, exp1)
            (expression2, l2) = modFuncExpression (table, exp2) 
            list = l1 ++ l2
            
    NEG exp1 -> (NEG expression1, list) where
            (expression1, list) = modFuncExpression (table, exp1)            
        
    CONST exp1 -> ((CONST exp1) , [])
        
    COND bexp1 exp1 exp2 -> ((COND bexpress express1 express2), list) where
            (bexpress,l1) = modFuncBexpression (table, bexp1)
            (express1,l2) = modFuncExpression (table, exp1)
            (express2,l3) = modFuncExpression (table, exp2)
            list = l1 ++ l2 ++ l3
        
modFuncBexpression:: (Table, (BExp String String)) -> ((BExp String String), [Fun String String])
modFuncBexpression (table, bexpr) = case bexpr of
    
    Lt exp1 exp2  -> (Lt expression1 expression2, list) where
        (expression1, l1) = modFuncExpression (table, exp1)
        (expression2, l2) = modFuncExpression (table, exp2)
        list = l1 ++ l2
    Gt exp1 exp2  -> (Lt expression1 expression2, list) where
        (expression1, l1) = modFuncExpression (table, exp1)
        (expression2, l2) = modFuncExpression (table, exp2)
        list = l1 ++ l2
    Eq exp1 exp2  -> (Lt expression1 expression2, list) where
        (expression1, l1) = modFuncExpression (table, exp1)
        (expression2, l2) = modFuncExpression (table, exp2)
        list = l1 ++ l2

    AND bexp1 bexp2 -> ((AND bexpression1 bexpression2), list) where
        (bexpression1, l1) = modFuncBexpression (table, bexp1)
        (bexpression2, l2) = modFuncBexpression (table, bexp2)
        list = l1 ++ l2        
    
    OR bexp1 bexp2 -> ((OR bexpression1 bexpression2), list) where
        (bexpression1, l1) = modFuncBexpression (table, bexp1)
        (bexpression2, l2) = modFuncBexpression (table, bexp2)
        list = l1 ++ l2
        
    NOT bexp1 -> ((NOT bexpression1), list) where
        ((bexpression1), list) = modFuncBexpression (table, bexp1) 

    TRUE -> (TRUE, [])
    FALSE -> (FALSE, [])
    
        
addExpressions::(Table, String) -> [Exp String String]
addExpressions ((ffs,callG), name) = convertVars vars1 where
    (n,args,vars1) = getFFItem name ffs 
    --vars = convertVars vars1
    
convertVars:: [String] -> [Exp String String]
convertVars list = map (\v -> VAR v) list

getFFArgs:: [FinalFunction] -> String -> [String]
getFFArgs ffList name = args where
    (n,args,vars) =  getFFItem name ffList 



