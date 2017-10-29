module  CallGraph where

import AlphaRename
import AST
import Data.List
import Data.Maybe

type CallGraph = [CallItem]
type CallItem = (String, [String])



produceCallGraph:: (Prog String String) -> ([Fun String String], CallGraph)
produceCallGraph (Prog prog) = (functions, callG) where
    functions = funcList (Prog prog)
    (callG, functions2) = processListOfFuncs ([], functions)
   
funcList :: (Prog String String) ->[Fun String String]
funcList (Prog prog) = functions where
    (symTab, (Prog functions)) = alphaRename (Prog prog)

--for testing
getSymTable:: (Prog String String) -> ST
getSymTable (Prog prog) = table where
    (table, (Prog functions)) = alphaRename (Prog prog)
    
processListOfFuncs:: (CallGraph, [Fun String String]) -> (CallGraph, [Fun String String])
processListOfFuncs (callG,[]) = (callG,[])
processListOfFuncs (callG, (f:funcs)) = (callGraph, functions) where
    (cg1, func1) = processFunc (callG, f)
    (cg2, funcs2) = processListOfFuncs (cg1, funcs)
    (callGraph, functions) =(cg2, (func1:funcs2))   
    
    
processFunc:: (CallGraph, (Fun String String)) -> (CallGraph, (Fun String String))
processFunc (callG, (Fun (name,args, exp))) = (callGraph, function) where
        cg2 = addFuncToCallGraph callG name        
        (callGraph, exp1) =  processExpression (cg2, exp)
        function = (Fun (name,args, exp1)) 
        
        
processExpression:: (CallGraph, Exp String String) -> (CallGraph, Exp String String)
processExpression (callG, exp) = case exp of 

    LET funcs exp1 -> (callGraph, expression) where      
        (cg1, expression) = processExpression(callG, exp1)
        (callGraph, fcns) = processListOfFuncs (cg1, funcs)               
        --(callGraph, expression) = processExpression (cg2, exp1)        
        
    APP name (e:exps) ->  (callGraph, expression) where
        (cg1) = updateCallGraph callG name
        (cg2, exp1) = processExpression (cg1, e)
        (callGraph, exp2) = processListOfExpressions (cg2,exps)
        expression = exp1
        
    VAR exp1 -> (callG, exp)       
     
    ADD exp1 exp2 -> (callGraph, expression) where
          (cg1, expression1) = processExpression (callG, exp1)
          (callGraph, expression) = processExpression (cg1, exp2)
          
    SUB exp1 exp2 -> (callGraph, expression) where
        (cg1, expression1) = processExpression (callG, exp1)
        (callGraph, expression) = processExpression (cg1, exp2)
    
    MUL exp1 exp2 -> (callGraph, expression) where
        (cg1, expression1) = processExpression (callG, exp1)
        (callGraph, expression) = processExpression (cg1, exp2)
    
    DIV exp1 exp2 -> (callGraph, expression) where
        (cg1, expression1) = processExpression (callG, exp1)
        (callGraph, expression) = processExpression (cg1, exp2)
    
    NEG exp1 -> (callGraph, expression) where
        (callGraph, expression) = processExpression (callG, exp1)
        
    CONST exp1 -> (callG, exp)     

    COND bexp1 exp1 exp2 -> (callGraph, expression)  where
        (cg1, bexpress1) = processBexpression (callG, bexp1)
        (cg2, express1) = processExpression (cg1, exp1)
        (callGraph, expression) =  processExpression (cg2, exp1)
  
  
processBexpression:: (CallGraph, BExp String String) -> (CallGraph, BExp String String)
processBexpression (callG, exp) = case (exp) of

    Lt exp1 exp2  -> (callGraph, Lt expression1 expression) where
        (cg1, expression1) = processExpression (callG, exp1)
        (callGraph, expression) = processExpression (cg1, exp2)         
        
    Gt exp1 exp2  -> (callGraph, Gt  expression1 expression) where
        (cg1, expression1) = processExpression (callG, exp1)
        (callGraph, expression) = processExpression (cg1, exp2)
        
    Eq exp1 exp2  -> (callGraph, Eq  expression1 expression) where
        (cg1, expression1) = processExpression (callG,exp1)
        (callGraph, expression) = processExpression (cg1, exp2)
        
    OR bexp1 bexp2  -> (callGraph, OR  expression1 expression) where
        (cg1, expression1) = processBexpression (callG, bexp1)
        (callGraph, expression) = processBexpression (cg1, bexp2)
        
    AND bexp1 bexp2  -> (callGraph, AND  expression1 expression) where
            (cg1, expression1) = processBexpression (callG, bexp1)
            (callGraph, expression) = processBexpression (cg1, bexp2)
        
    NOT bexp1 -> (callGraph, NOT expression) where
            (callGraph, expression) = processBexpression (callG, bexp1)                             
          
    TRUE  -> (callG, TRUE)    
    FALSE -> (callG, FALSE)
 

updateCallGraph :: CallGraph -> String -> CallGraph
updateCallGraph [] name = case (name == "f0") of
    True  -> [(name,[])]
    False -> error "Malformed Program"
updateCallGraph ((func,list):cg) name = case (name `elem` list) of 
    True -> ((func,list):cg)
    False -> ((func,name:list):cg)
    
addFuncToCallGraph :: CallGraph -> String -> CallGraph
addFuncToCallGraph [] name = (name, []):[]
addFuncToCallGraph graph name = case (name `elem` (map fst graph)) of 
    True -> outGraph where
        outGraph = pushToFront name graph
    False -> (name,[]):graph

letUpdate:: CallGraph -> [Fun String String] -> CallGraph
letUpdate ((nm,list):cg) functions = graph where

    names = listOfNames functions   
    graph = ((nm, list1):cg)
    list1 = (mergeUnique names list)
    --graph = ((nm, names ++ list1):cg) -- remove duplicates from list concat

getGraphIndex:: String -> CallGraph -> Int
getGraphIndex item list = fromJust (elemIndex item (map fst list))


pushToFront:: String -> CallGraph -> CallGraph
pushToFront name cGraph = graph where
    (frontGraph, ((b,lst):backGraph)) = splitAt (getGraphIndex name cGraph) cGraph
    graph = (name,lst):(frontGraph ++ backGraph)
    
    
processListOfExpressions:: (CallGraph, [Exp String String]) -> (CallGraph, [Exp String String])
processListOfExpressions (callG, []) = (callG,[])
processListOfExpressions (callG, (e:exps)) = (callGraph,expressions) where
    (cg1, exp1) = processExpression (callG, e)
    (cg2, exprs) = processListOfExpressions (cg1,exps)
    (callGraph, expressions) = (cg2,((exp1):exprs))
    
mergeUnique:: [String] -> [String] -> [String]
mergeUnique l [] = l
mergeUnique [] l = l
mergeUnique (l:list1) list2 = case (l `elem` list2) of 
    True -> mergeUnique list1 list2
    False -> mergeUnique list1 (l:list1)
listOfNames:: [Fun String String] -> [String]
listOfNames funcs = map (\(Fun (name,_,_)) -> name) funcs
