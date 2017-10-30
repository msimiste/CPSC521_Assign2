module LambdaLift where

import CallGraph
import AlphaRename
import AST
import Data.List
import Data.Maybe

type FreeVars = [String]
type Arguments = [String]
type Name = String

type FinalFunction = (Name, Arguments,FreeVars)

type Table = ([FinalFunction],CallGraph)


lambdaLift:: Table -> (Prog String String) -> Table
lambdaLift inTable prog = table where
    (alphaFuncs, callGraph) = produceCallGraph prog
    table = parseFunctions (inTable, alphaFuncs)

parseFunctions:: (Table,[Fun String String]) -> (Table)
parseFunctions (f:funcs) = table where
    (t1) = parseFunction table f
    (t2) = parseFunctions funcs
    table = t2
    
parseFunction (Table, Fun String String) -> Table 
parseFunction inTable func = outTable where
        table1 = addFuncToTable inTable func        
        vars = parseExpression func exp
        

lambdaExpression:: (Table, (Fun String String), (Exp String String)) -> Table
lambdaExpression inTable (Fun (name,args,e)) exp = case exp of

    LET funcs exp1 -> table where
        table1 = lambdaFunctions inTable funcs
        table = lambdaExpression (table1, (Fun(name,args,e)), exp)  
          
    APP name exp1 -> table where

    VAR exp1 -> updateTableFreeVar name exp1
        
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
    
    CONST exp1 = table
    
    COND bexp1 exp1 exp2 = table where
        table1 = lambdaBexpression (inTable, (Fun (name,args,e)),bexp1)
        table2 =  lambdaExpression (table1, (Fun (name,args,e)),exp1)
        table = lambdaExpression (table2, (Fun (name, args, e)),exp2)
        
lambdaBexpression:: (Table, (Fun String String), (BExp String String)) -> Table
lambdaBexpression inTable func exp = case exp of

    Lt exp1 exp2  -> (table1) where
        (tb1) = lambdaExpression (table,func, exp1)
        (table1) = lambdaExpression (tb1,func, exp2)         
        
    Gt exp1 exp2  -> (table1) where
        (tb1) = lambdaExpression (table, func, exp1)
        (table1) = lambdaExpression (tb1, func, exp2)
        
    Eq exp1 exp2  -> (table1) Eq  expression1 expression) where
        (tb1)  = lambdaExpression (table, exp1)
        (table1)  = lambdaExpression (tb1, exp2)
        
    OR exp1 exp2  -> (table1) OR  expression1 expression) where
        (tb1)  = lambdaBexpression (table, exp1)
        (table1)  = lambdaBexpression (tb1, exp2)
        
    AND exp1 exp2  -> (table1) AND  expression1 expression) where
            (tb1)  = lambdaBexpression (table, exp1)
            (table1)  = lambdaBexpression (tb1, exp2)
        
    NOT exp1 -> (table1) NOT expression) where
            (table1) = lambdaBexpression (table, exp1)                             
          
    TRUE  -> (table)    
    FALSE -> (table)

     
updateTableFreeVar:: Table -> String -> String -> Table
updateTableFreeVar (ffList, cList) name var = table1 where
    table1 = updateFFList name var
    
updateFFList:: [FinalFunction] -> String -> String -> [String]
updateFFList ffList name var = map (\(n,args,vars) -> case (n == name) of 
    True -> (n,args,(var:vars))
    False -> (n,args,vars))ffList
    
addFuncToTable :: Table -> FinalFunction-> Table
addFuncToTable ([],[]) (name, args,vList)= ((name,args,vList):[],[])
addFuncToTable  (ffList,cList) (name,args,vList) = case (name `elem` (getTableNames (ffList,cList))) of 
    True -> (ffList,cList)
    False -> (((name,args,vList):ffList),cList)
    
--    table = getFinalFunctionNames callGraph

--getFinalFunctionNames:: CallGraph -> Table
--getFinalFunctionNames callGraph = map (\(name,_) -> (name,[],[],[])) callGraph
--
--
--firstPass:: (Table,[Fun String String]) -> (Table, [Fun String String]) 
--firstPass funcs inTable = outTable where
--    processFuncs 
--    
--processFuncs:: (Table, [Fun String String]) -> (Table, [Fun String String])
--processFuncs (table, funcs) = outTable where
--    
--    
--
--processFunction:: (FinalFunction, (Fun String String)) -> (FinalFunction)
--processFucntion table (Fun (name,args,_)) = outTable where
--    table1 = (FinalFunction(name,args,[],[])):table
--    
--    
--
--addArgsToFunction:: FinalFunction -> [String] -> FinalFunction
--addArgsToFunction (FinalFunction(name,_,_._)) args = (FinalFunction(name,args,_,_))   
--    
--getFunction:: Name -> Table -> FinalFunction
--getFunction name [] = error "Empty Table"
--getFunction name table = finalFunction where
--    index = getFunctionIndex name table
--    finalFunction = table !! index 
--
--getFunctionIndex:: Name -> Table -> Int
--getFunctionIndex name table = fromJust (elemIndex name (map (\(n,_,_,_) -> n) table))
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

-- namesAndArgs:: [Fun String String] -> [(String,[String])]
-- namesAndArgs funcs = finalList where
--     names = map (\(Fun (name,_,_)) -> name) funcs
--     arguments = map(\(Fun (_,args,_)) -> args) funcs
--     finalList = zip names arguments
--    
--
