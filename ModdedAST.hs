module ModdedAST where

import CallGraph
import AlphaRename
import LambdaLift
import AST
import Data.List
import Data.Maybe
import TestFiles




modAST:: (Prog String String) -> (Prog String String)
modAST prog = Prog outFunctions where
    table = lambdaLift prog
    (functions, callG) = produceCallGraph prog
    (outTable, outFunctions) = modListOfFuncs (table,functions,[])
    
    
    
modListOfFuncs:: (Table, [Fun String String], [Fun String String]) -> (Table, [Fun String String])
modListOfFuncs (tableT,[], list) = (tableT,list)
modListOfFuncs (tableT, (f:funcs), list) = (table, functions) where
    (cg1, funcs1) = modFunc (tableT, f, list)
    (cg2, funcs2) = modListOfFuncs (cg1, funcs, funcs1)
    (table, functions) = (cg2, funcs1 ++ funcs2)   
    
    
modFunc:: (Table, (Fun String String), [Fun String String]) -> (Table, [Fun String String])
modFunc (tableT, (Fun (name,args, exp)), list) = case (exp) of
    LET funs exp -> (tableT, (fun:list)) where
        (tb, functions) = (modListOfFuncs (tableT,funs,(fun:list)))  --(tableT, (fun:list))where
        fun = (Fun (name,args,exp))
         --(t2,functions) = modListOfFuncs (tableT, funs, list)   
         --fun = (Fun (name,args,exp))
         
    APP n exps -> (tableT,functions) where
        addedList = addExpressions (tableT,n)
        functions = ((Fun (name, args, (APP n (exps ++ addedList)))):list)
    
    _ -> (tableT, ((Fun (name, args, exp))):list)
   

     
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
    --(table,function) where
    --(table, function, exp1) = modExpression (tableT, exp)
    --function = (Fun (name,args, exp1)) 
        
        
--modExpression:: (Table, Fun String String , Exp String String, ) -> (Table, Fun String String, Exp String String)
--modExpression (tableT, func, exp) = case exp of 

    --LET funcs exp1 -> (table, outFunc, expression1) where
      --(cg1, fcns) = modListOfFuncs (tableT, funcs) 
      --(table, outFunc, expression1) = modExpression(cg1, func, exp1)
                    
        ----(table, expression) = modExpression (cg2, exp1)        
        
    --APP name exps ->  (tableT, APP name expressions) where
        --addedList = addExpressions (table,name)
        --(table, (expressions)) = modListOfExpressions (tableT, (exps ++ addedList))
                
    --VAR exp1 -> (tableT, func, VAR exp)       
     
    --ADD exp1 exp2 -> (table, outFunc, expression) where
          --(cg1, outFunc1, expression1) = modExpression (tableT, func, exp1)
          --(table, outFunc, expression) = modExpression (cg1, outFunc1, exp2)
          
    --SUB exp1 exp2 -> (table, outFunc, expression) where
        --(cg1, outFunc1, expression1) = modExpression (tableT, func, exp1)
        --(table, outFunc, expression) = modExpression (cg1, outFunc1, exp2)
    
    --MUL exp1 exp2 -> (table, outFunc, expression) where
        --(cg1, outFunc1, expression1) = modExpression (tableT, func, exp1)
        --(table, outFunc, expression) = modExpression (cg1, outFunc1, exp2)
    
    --DIV exp1 exp2 -> (table, outFunc, expression) where
        --(cg1,outFunc1 expression1) = modExpression (tableT, func, exp1)
        --(table, outFunc, expression) = modExpression (cg1, outFunc1, exp2)
    
    --NEG exp1 -> (table, outFunc, expression) where
        --(table, outFunc, expression) = modExpression (tableT, func, exp1)
        
    --CONST exp1 -> (tableT, exp)     

    --COND bexp1 exp1 exp2 -> (table, outFunc, expression)  where
        --(cg1, outFunc1, bexpress1) = modBexpression (tableT, func, bexp1)
        --(cg2, outFunc2, express1) = modExpression (cg1, outFunc1, exp1)
        --(table,outFunc expression) =  modExpression (cg2, outFunc2, exp1)
  
  
--modBexpression:: (Table, Fun String String,  BExp String String) -> (Table, Fun String String, BExp String String)
--modBexpression (tableT, func, exp) = case (exp) of

    --Lt exp1 exp2  -> (table, outFunc, Lt expression1 expression) where
        --(cg1, outFunc1, expression1) = modExpression (tableT, func, exp1)
        --(table, outFunc, expression) = modExpression (cg1, outFunc1, exp2)         
        
    --Gt exp1 exp2  -> (table, outFunc, Gt  expression1 expression) where
        --(cg1, outFunc1, expression1) = modExpression (tableT, func, exp1)
        --(table, outFunc, expression) = modExpression (cg1, outFunc1, exp2)
        
    --Eq exp1 exp2  -> (table, outFunc, Eq  expression1 expression) where
        --(cg1, outFunc1, expression1) = modExpression (tableT, func, exp1)
        --(table, outFunc, expression) = modExpression (cg1, outFunc1, exp2)
        
    --OR bexp1 bexp2  -> (table, outFunc, OR  expression1 expression) where
        --(cg1, outFunc1, expression1) = modBexpression (tableT, func,bexp1)
        --(table, outFunc, expression) = modBexpression (cg1, outFunc1, bexp2)
        
    --AND bexp1 bexp2  -> (table, outFunc, AND  expression1 expression) where
            --(cg1, outFunc1, expression1) = modBexpression (tableT, func, bexp1)
            --(table,outFunc expression) = modBexpression (cg1, outFunc1, bexp2)
        
    --NOT bexp1 -> (table, outFunc, NOT expression) where
            --(table, outFunc, expression) = modBexpression (tableT,func, bexp1)                             
          
    --TRUE  -> (tableT, outFunc, TRUE)    
    --FALSE -> (tableT, outFunc, FALSE)
 
--modListOfExpressions:: (Table, [Exp String String]) -> (Table, [Exp String String])
--modListOfExpressions (tableT, []) = (tableT,[])
--modListOfExpressions (tableT, (e:exps)) = (table,expressions) where
--    (cg1, exp1) = modExpression (tableT, e)
--    (cg2, exprs) = modListOfExpressions (cg1,exps)
--    (table, expressions) = (cg2,((exp1):exprs))    
        
addExpressions::(Table, String) -> [Exp String String]
addExpressions ((ffs,callG), name) = vars where
    (n,args,vars1) = getFFItem name ffs 
    vars = convertVars vars1
    
convertVars:: [String] -> [Exp String String]
convertVars list = map (\v -> VAR v) list
