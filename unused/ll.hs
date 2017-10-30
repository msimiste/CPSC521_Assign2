

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




lambdaLift:: (Prog String String) -> CallGraph
lambdaLift prog = callGraph where
    (alphaFuncs, callGraph) = produceCallGraph prog
    (table,funs) = lambdaListOfFuncs (([],callGraph), alphaFuncs)
   

lambdaListOfFuncs:: (Table, [Fun String String]) -> (Table, [Fun String String])
lambdaListOfFuncs (table,[]) = (table,[])
lambdaListOfFuncs (table, (f:funcs)) = (table1, functions) where   
    (cg1, func1) = lambdaFunc (table, f)
    (cg2, funcs2) = lambdaListOfFuncs (cg1, funcs)
    (table1, functions) =(cg2, (func1:funcs2))   
    
    
lambdaFunc:: (Table, (Fun String String)) -> (Table, (Fun String String))
lambdaFunc (table, (Fun (name,args, exp))) = (table1, function) where        
        cg1 = addFuncToTable table (funcToFF (name, args))
        function = (Fun (name,args, exp1)) 
        (table1, exp1) =  lambdaExpression (cg1, exp)
        
        
lambdaExpression:: (Table, Exp String String) -> (Table, Exp String String)
lambdaExpression (table, exp) = case exp of 

    LET funcs exp1 -> (table, expression) where       
        --tb1 = lambdaLetUpdate table funcs         
        (tb2, fcns) = lambdaListOfFuncs (table, funcs)               
        (table, expression) = lambdaExpression (tb2, exp1)        
        
    --APP name (e:exps) ->  (table1, expression) where
    --    (cg1) = updatetable table name
    --    (cg2, exp1) = lambdaExpression (cg1, e)
    --    (table1, exp2) = lambdaListOfExpressions (cg2,exps)
    --    expression = exp1
        
    VAR exp1 -> (table, exp)       
     
    ADD exp1 exp2 -> (table1, expression) where
          (cg1, expression1) = lambdaExpression (table, exp1)
          (table1, expression) = lambdaExpression (cg1, expression1)
          
    SUB exp1 exp2 -> (table1, expression) where
        (cg1, expression1) = lambdaExpression (table, exp1)
        (table1, expression) = lambdaExpression (cg1, expression1)
    
    MUL exp1 exp2 -> (table1, expression) where
        (cg1, expression1) = lambdaExpression (table, exp1)
        (table1, expression) = lambdaExpression (cg1, expression1)
    
    DIV exp1 exp2 -> (table1, expression) where
        (cg1, expression1) = lambdaExpression (table, exp1)
        (table1, expression) = lambdaExpression (cg1, expression1)
    
    NEG exp1 -> (table1, expression) where
        (table1, expression) = lambdaExpression (table, exp1)
        
    CONST exp1 -> (table, exp)     

    COND bexp1 exp1 exp2 -> (table1, expression)  where
        (cg1, bexpress1) = lambdaBexpression (table, bexp1)
        (cg2, express1) = lambdaExpression (cg1, exp1)
        (table1, expression) =  lambdaExpression (cg2, exp1)
  
  
lambdaBexpression:: (Table, BExp String String) -> (Table, BExp String String)
lambdaBexpression (table, exp) = case (exp) of

    Lt exp1 exp2  -> (table1, Lt expression1 expression) where
        (tb1, expression1) = lambdaExpression (table, exp1)
        (table1, expression) = lambdaExpression (tb1, exp2)         
        
    Gt exp1 exp2  -> (table1, Gt  expression1 expression) where
        (tb1, expression1) = lambdaExpression (table, exp1)
        (table1, expression) = lambdaExpression (tb1, exp2)
        
    Eq exp1 exp2  -> (table1, Eq  expression1 expression) where
        (tb1, expression1) = lambdaExpression (table, exp1)
        (table1, expression) = lambdaExpression (tb1, exp2)
        
    OR exp1 exp2  -> (table1, OR  expression1 expression) where
        (tb1, expression1) = lambdaBexpression (table, exp1)
        (table1, expression) = lambdaBexpression (tb1, exp2)
        
    AND exp1 exp2  -> (table1, AND  expression1 expression) where
            (tb1, expression1) = lambdaBexpression (table, exp1)
            (table1, expression) = lambdaBexpression (tb1, exp2)
        
    NOT exp1 -> (table1, NOT expression) where
            (table1, expression) = lambdaBexpression (table, exp1)                             
          
    TRUE          -> (table, TRUE)    
    FALSE         -> (table, FALSE)
 

--updateTable :: Table -> String -> Table
--updateTable [] name = error "Malformed Program"
--updateTable ((func,list):cg) name = case (name `elem` list) of 
--    True -> ((func,list):cg)
--    False -> ((func,name:list):cg)
    
addFuncToTable :: Table -> FinalFunction-> Table
addFuncToTable ([],[]) (name, args,vList)= ((name,args,vList):[],[])
addFuncToTable  (ffList,cList) (name,args,vList) = case (name `elem` (getTableNames (ffList,cList))) of 
    True -> (ffList,cList)
    False -> (((name,args,vList):ffList),cList)
    
funcToFF:: (String, [String]) -> FinalFunction
funcToFF (name,args) = (name,args,[])

getTableNames:: Table -> [String]
getTableNames (ffList, cList) = map (\(name,_,_) -> name) ffList

namesAndArgs:: [Fun String String] -> [(String,[String])]
namesAndArgs funcs = finalList where
    names = map (\(Fun (name,_,_)) -> name) funcs
    arguments = map(\(Fun (_,args,_)) -> args) funcs
    finalList = zip names arguments

lambdaLetUpdate:: Table -> [Fun String String] -> Table
lambdaLetUpdate (ffList,cList) functions = table1 where
        nArgs = namesAndArgs functions
        table1 = foldl(\acc nameArg -> addFuncToTable acc (funcToFF nameArg)) (ffList,cList) nArgs

   
