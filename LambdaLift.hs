module LambdaLift where

import CallGraph
import AlphaRename
import AST
import Data.List
import Data.Maybe
import TestFiles

type FreeVars = [String]
type Arguments = [String]
type Name = String

type FinalFunction = (Name, Arguments,FreeVars)

type Table = ([FinalFunction],CallGraph)


lambdaLift:: Table -> (Prog String String) -> Table
lambdaLift inTable prog = table where
    (alphaFuncs, callGraph) = produceCallGraph prog
    table = lambdaFunctions (inTable, alphaFuncs)

lambdaFunctions:: (Table,[Fun String String]) -> (Table)
lambdaFunctions (tble,[]) = tble
lambdaFunctions (inTable,(f:funcs)) = table where
    (t1) = lambdaFunction (inTable, f)
    (t2) = lambdaFunctions (t1,funcs)
    table = t2
    
lambdaFunction:: (Table, Fun String String) -> Table 
lambdaFunction (inTable, (Fun (name,args,exp))) = outTable where
        table1 = addFuncToTable inTable (funcToFF func)     
        outTable = lambdaExpression (table1, func, exp)
        func = (Fun (name,args,exp))        

lambdaExpression:: (Table, (Fun String String), (Exp String String)) -> Table
lambdaExpression (inTable, (Fun (name,args,e)), exp) = case exp of

    LET funcs exp1 -> table where
        table1 = lambdaFunctions (inTable, funcs)
        table = lambdaExpression (table1, (Fun(name,args,e)), exp1)  
          
    APP name exp1 -> inTable 

    VAR exp1 -> table where
        table = updateTableFreeVar inTable (name, exp1)
        
    ADD exp1 exp2 -> table where
        table1 = lambdaExpression (inTable, (Fun (name,args,e)),exp1)
        table =  lambdaExpression (table1, (Fun (name,args,e)),exp2)
    
    SUB exp1 exp2 -> table where
        table1 = lambdaExpression (inTable, (Fun (name,args,e)),exp1)
        table =  lambdaExpression (table1, (Fun (name,args,e)),exp2)
            
    MUL exp1 exp2 -> table where
        table1 = lambdaExpression (inTable, (Fun (name,args,e)),exp1)
        table =  lambdaExpression (table1, (Fun (name,args,e)),exp2)

    DIV exp1 exp2 -> table where
        table1 = lambdaExpression (inTable, (Fun (name,args,e)),exp1)
        table =  lambdaExpression (table1, (Fun (name,args,e)),exp2)
         
    NEG exp1 -> table where
        table = lambdaExpression (inTable, (Fun (name,args,e)),exp1)   
    
    CONST exp1 ->  inTable
    
    COND bexp1 exp1 exp2 -> table where
        table1 = lambdaBexpression (inTable, (Fun (name,args,e)),bexp1)
        table2 =  lambdaExpression (table1, (Fun (name,args,e)),exp1)
        table = lambdaExpression (table2, (Fun (name, args, e)),exp2)
        
lambdaBexpression:: (Table, (Fun String String), (BExp String String)) -> Table
lambdaBexpression (inTable, func, exp) = case exp of
    Lt exp1 exp2  -> (table1) where
        (tb1) = lambdaExpression (inTable,func, exp1)
        (table1) = lambdaExpression (tb1,func, exp2)         
        
    Gt exp1 exp2  -> (table1) where
        (tb1) = lambdaExpression (inTable, func, exp1)
        (table1) = lambdaExpression (tb1, func, exp2)
        
    Eq exp1 exp2  -> (table1) where
        (tb1)  = lambdaExpression (inTable,func, exp1) 
        (table1)  = lambdaExpression (tb1,func, exp2)  
                                                  
    OR exp1 exp2 -> (table1) where
        (tb1)  = lambdaBexpression (inTable,func, exp1)
        (table1) = lambdaBexpression (tb1, func, exp2) 
                                                  
    AND exp1 exp2  -> (table1) where
            (tb1)  = lambdaBexpression (inTable, func, exp1)
            (table1)  = lambdaBexpression (tb1,func, exp2)
        
    NOT exp1 -> (table1) where
            (table1) = lambdaBexpression (inTable, func, exp1)                             
          
    TRUE  -> (inTable)    
    FALSE -> (inTable)

     
updateTableFreeVar:: Table -> (String,String) -> Table
updateTableFreeVar (ffList, cList) (name,var) = table1 where
    newList = updateFFList ffList name var
    table1 = (newList,cList)
    
updateFFList:: [FinalFunction] -> String -> String -> [FinalFunction]
updateFFList ffList name var = map (\(n,args,vars) -> case (n == name) of 
    True -> case (var `elem` vars) of 
        True -> (n,args,vars)
        False -> (n,args,(var:vars))
    False -> (n,args,vars)) ffList
    
addFuncToTable :: Table -> FinalFunction-> Table
addFuncToTable ([],[]) (name, args,vList)= ((name,args,vList):[],[])
addFuncToTable  (ffList,cList) (name,args,vList) = case (name `elem` (getTableNames (ffList,cList))) of 
    True -> (ffList,cList)
    False -> (((name,args,vList):ffList),cList)

funcToFF:: (Fun String String) -> FinalFunction
funcToFF (Fun (name,args,e)) = (name,args,[])


getTableNames:: Table -> [String]
getTableNames (ffList, cList) = map (\(name,_,_) -> name) ffList



--
--
----start::
----[Fun String String]
----1 get listOf functions
----2. make list of final functions
----3. populate call graph
----    globally or locally
----4. create a function that updates freevars for each FinalFunction
----5. apply function from 4 to the list of final functions
----5. compare list of final functions to previous list of Final functions
----6. if step 5 is such that both lists are equal, stop otherwise goto step 4

--table compare
    -- start with a Table of initial values (obtained from the lambdaLift function    
    -- runUpdateVars on the table
        --updateVars:: Table -> Table
        -- updateVars does the following:
        --for function_(n0 in the list of FinalFunctions
            -- for each function_(y) that calls function_(n) , add the freevariables of function_(n) to functions_(y)'s free variable list
        -- send Table to tableCompare function.
        --tableCompare:: Table -> Bool
        -- if tableCompare == True then return table else run updateVars on table again.
        
        --tableCompare
