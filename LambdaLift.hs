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


lambdaLift:: (Prog String String) -> Table
lambdaLift prog = table where
    (alphaFuncs, callGraph) = produceCallGraph prog
    table1 = lambdaFunctions (([],callGraph), alphaFuncs)
    table2 = updateVars table1
    (ffs,cg) = removeArgs table2
    functions = fixArgs ffs
    table = (functions, cg)
   

removeArgs:: Table -> Table
removeArgs (ffList, cg) = outTable where
    ffs = parseArgs ffList
    outTable = (ffs, cg)
    
parseArgs:: [FinalFunction] -> [FinalFunction]
parseArgs ffList = outFF where
    outFF = map parseArg ffList 

parseArg:: FinalFunction -> FinalFunction
parseArg finalF = (name,args,vars1) where
    vars1 = extractVars finalF 
    (name, args, vars) = finalF
    
lambdaFunctions:: (Table,[Fun String String]) -> (Table)
lambdaFunctions (tble,[]) = tble
lambdaFunctions (inTable,(f:funcs)) = table where
    (t1) = lambdaFunction (inTable, f)
    (t2) = lambdaFunctions (t1,funcs)
    table = t2
    
lambdaFunction:: (Table, Fun String String) -> Table 
lambdaFunction (inTable, (Fun (name,args,exp))) = outTable where
        table1 = addFuncToTable inTable (funcToFF (Fun(name,args,exp)))     
        outTable = lambdaExpression (table1, (Fun(name,args,exp)), exp)
        --func = (Fun (name,args,exp)) 
            
letLambdaFunctions:: (Table, [Fun String String]) -> Table
letLambdaFunctions (table, []) = table
letLambdaFunctions (table, (f:funs)) = outTable where
    tb1 = letLambdaFunction (table , f)
    outTable = letLambdaFunctions (tb1, funs)

letLambdaFunction:: (Table, (Fun String String)) -> Table
letLambdaFunction (table, (Fun (name, args, exp))) = outTable where
    outTable = lambdaExpression (table, (Fun (name, args, exp)), exp)


lambdaExpression:: (Table, (Fun String String), (Exp String String)) -> Table
lambdaExpression (inTable, (Fun (name,args,e)), exp) = case exp of

    LET funcs exp1 -> table2 where       
            table = lambdaExpression (inTable, (Fun(name,args,e)), exp1) 
            (table1, functions) =  lambdaLetFunctions (table, funcs)--lambdaFunctions (inTable, funcs) 
            (table2) = letLambdaFunctions (table1, functions)
            
    APP fName exp1 -> table where
        table1 = handleApp (inTable, (Fun (name,args,e)), exp1)-- updateTableFreeVar inTable (name, exp1)
        (table) = listOfLambdaExpressions (table1, (Fun(name,args,e)), exp1)

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



handleApp:: (Table, (Fun String String), [(Exp String String)]) -> Table
handleApp (inTable, func, []) = inTable
handleApp (inTable, func, (e:exps)) = table where
    tb1 = lambdaExpression (inTable, func, e)
    table = handleApp (tb1, func, exps)
    
lambdaLetFunctions:: (Table, [Fun String String]) -> (Table, [Fun String String])
lambdaLetFunctions (table, []) = (table, [])
lambdaLetFunctions (table, (fun:funcList)) = (outTable, (fun:funcList)) where
     tb1 = addFuncToTable table (funcToFF fun)
     (outTable, functions) = lambdaLetFunctions (tb1, funcList)
    
listOfLambdaExpressions:: (Table, (Fun String String), [(Exp String String)]) -> Table
listOfLambdaExpressions (table, func, []) = table
listOfLambdaExpressions (table, func, (e:exps)) = outTable where
    (tb1) = lambdaExpression (table, func, e)
    (outTable) = listOfLambdaExpressions (tb1, func, exps)
    
     
updateTableFreeVar:: Table -> (String,String) -> Table
updateTableFreeVar (ffList, cList) (name,var) = table1 where
    newList = updateFFList ffList name var
    table1 = (newList,cList)
    
--updateFFList:: [FinalFunction] -> String -> String -> [FinalFunction]
--updateFFList ffList name var = map (\(n,args,vars) -> case (n == name) of 
--    True -> case (var `elem` vars) of 
--        True -> (n,args,vars) --error "The var is: " ++ var--
--        False ->  (n,args,(var:vars)) --error "Var plus list: " ++ (var:vars)--
--    False -> (n,args,vars)) ffList
 
 
updateFFList:: [FinalFunction] -> String -> String -> [FinalFunction]
updateFFList [] name var = []
updateFFList (ff:ffList) name var = functions where
    (fun1) = updateFFSingle ff name var
    (funs1) = updateFFList ffList name var
    functions = (fun1:funs1)
    
updateFFSingle:: FinalFunction -> String -> String -> FinalFunction
updateFFSingle (nm,args,vars) name var = case (name == nm) of
        True -> case (var `elem` vars) of
            True -> finalFunction where
                finalFunction = (nm,args,vars)
            False -> finalFunction where
                finalFunction = (nm, args, (var:vars)) 
        False -> (nm,args,vars)

getFFIndex:: String -> [FinalFunction] -> Int
getFFIndex item list = fromJust (elemIndex item (map (\(name,args,vars) -> name) list))

getFFItem:: String -> [FinalFunction] -> FinalFunction
getFFItem name list = finalFunc where
    index = getFFIndex name list
    finalFunc = list !! index            
            
addFuncToTable :: Table -> FinalFunction-> Table
addFuncToTable ([],[]) (name, args,vList)= ((name,args,vList):[],[])
addFuncToTable  (ffList,cList) (name,args,vList) = case (name `elem` (getTableNames (ffList,cList))) of 
    True -> (ffList,cList)
    False -> (((name,args,vList):ffList),cList)

funcToFF:: (Fun String String) -> FinalFunction
funcToFF (Fun (name,args,e)) = (name,args,[])


getTableNames:: Table -> [String]
getTableNames (ffList, cList) = map (\(name,_,_) -> name) ffList


--Table Compare Section
tableCompare:: Table -> Table -> Bool
tableCompare table1 table2 = (table1 == table2)

updateVars:: Table -> Table
updateVars inTable = outTable where
    tempTable = updateFinalFunctions inTable
    outTable = case (tableCompare inTable tempTable) of
        True -> tempTable
        False -> updateVars tempTable
       
    
updateFinalFunctions:: Table -> Table
updateFinalFunctions inTable = outTable where
    finals = updateFinals ffs inTable    
    (ffs,callG) = inTable
    outTable = (finals,callG)
  
updateFinals:: [FinalFunction] -> Table -> [FinalFunction]
updateFinals []  tbl = []
updateFinals (f:ffs) inTable = functions where
    func1 = updateFinalFunction f inTable
    funcs1 = updateFinals ffs inTable
    functions = (func1:funcs1)
   
updateFinalFunction:: FinalFunction -> Table -> FinalFunction
updateFinalFunction inFF inTable = finalOut where
    funcsThatICall = getFuncsIcall name callG
    finalFuncsThatICall = map (\n -> getFFItem n ffs ) funcsThatICall
    finalOut = updateFFVars inFF finalFuncsThatICall 
    (ffs, callG) = inTable
    (name,args,freeVars) = inFF
    
getFuncsIcall:: String -> CallGraph -> [String]
getFuncsIcall name callG = funcs where
    index = getGraphIndex name callG
    (nm,funcs) = callG !! index

updateFFVars:: FinalFunction -> [FinalFunction] -> FinalFunction
updateFFVars inFF ffList = outFF where
        outFF = (name, args, freeVars)
        (name,args,frees) = inFF
        freeVars1 = getAllFreeVars ffList
        freeVars = parseVars (frees ++ freeVars1)
        
getAllFreeVars:: [FinalFunction] -> [String]
getAllFreeVars [] = []
getAllFreeVars (f:ffs) = freeVars where
    var1 = extractVars f
    var2 = getAllFreeVars ffs
    freeVars =  (var1 ++ var2)

--parseVarsDouble:: [String] -> [String] -> [String]
--parseVarsDouble [] [] = []
--parseVarsDouble a [] = oarseVarsSingle a
--parseVarsDouble [] a = parseVarsSingle a
--parseVarsDouble (l:list1) (list2) = list where
--    list = case (l `elem` list1) of 
--        True -> parseVarsDouble list1 list2
--        False -> parseVarsDouble list1 (l:list2)
fixArgs:: [FinalFunction] -> [FinalFunction]
fixArgs ffList = outList where
    outList = map (\(name, args, vars) -> (name, args ++ (vars \\ args), vars)) ffList
parseVars:: [String] -> [String]
parseVars [] = []
parseVars [c] = [c]
parseVars (l:list1) = l:(parseVars (filter (/= l) list1))  -- case (l `elem` list1) of
    --True -> parseVarsSingle list1
    --False -> l:(parseVarsSingle list1)

extractVars:: FinalFunction -> [String]
extractVars (name,args,vars) = filter (`notElem` args) vars
--varsOut where
--    varsOut = map(\var -> case (var `elem` args) of
--        True -> ""
--        False -> var) vars
    
--flatten( map (\(nm,list) -> case (name == nm) of 
--    True -> list
--    False -> []) callG)
            
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
        --for function_(n) in the list of FinalFunctions
            -- for each function_(y) that calls function_(n) , add the freevariables of function_(n) to functions_(y)'s free variable list
        -- send Table to tableCompare function.
        --tableCompare:: Table -> Bool
        -- if tableCompare == True then return table else run updateVars on table again.
        
        --tableCompare
