import AlphaRename
import AST

type CallGraph = [CallItem]
type CallItem = (String, [String])
type FreeVars = [String]
type Arguments = [String]
type Name = String

type FinalFunction = (Name, Arguments,FreeVars, CallGraph)
data MExp a b = BExp String String | Exp String String deriving Show

produceFinal:: (Prog String String) -> CallGraph
produceFinal (Prog prog) = callG where
    functions1 = funcList (Prog prog)
    (callG) = processListOfFuncs ([], functions1)
    --finalNameArgs = map getNameArgs functions1
    --finalList = finalNameArgs
    --
funcList :: (Prog String String) ->[Fun String String]
funcList (Prog prog) = functions where
    (Prog functions) = alphaRename (Prog prog)


processListOfFuncs:: (CallGraph, [Fun String String]) -> (CallGraph)
processListOfFuncs (callG,[]) = (callG,[])
processListOfFuncs (callG, (f:funcs)) = (callGraph) where
    (cg1) = processFunc (callG, f)
    (cg2) = processListOfFuncs (cg1)
    (callGraph) =(cg2) 
    
    
processFunc:: (CallGraph, (Fun String String)) -> (CallGraph)
processFunc (callG, (Fun (name,args, exp))) = (callGraph) where        
        cg1 = addFuncToCallGraph callG name        
        (callGraph) =  processExpression (cg1, exp)
        
        
processExpression:: (CallGraph, Exp String String) -> (CallGraph)
processExpression (callG, exp) = case exp of 

    LET funcs exp1 -> (callGraph) where       
        call = letUpdate callG funcs         
        (cg2, fcns) = processListOfFuncs (call, funcs)               
        (callGraph) = processExpression (cg2, exp1)        
        
    APP name (e:exps) ->  (callGraph) where
        (cg1) = updateCallGraph callG name
        (cg2) = processExpression (cg1, e)
        (callGraph) = processListOfExpressions (cg2,exps)
        
        
    VAR exp1 -> (callG)       
     
    ADD exp1 exp2 -> (callGraph) where
          (cg1, expression1) = processExpression (callG, exp1)
          (callGraph) = processExpression (cg1, expression1)
          
    SUB exp1 exp2 -> (callGraph) where
        (cg1, expression1) = processExpression (callG, exp1)
        (callGraph) = processExpression (cg1, expression1)
    
    MUL exp1 exp2 -> (callGraph) where
        (cg1, expression1) = processExpression (callG, exp1)
        (callGraph) = processExpression (cg1, expression1)
    
    DIV exp1 exp2 -> (callGraph) where
        (cg1) = processExpression (callG, exp1)
        (callGraph) = processExpression (cg1, expression1)
    
    NEG exp1 -> (callGraph) where
        (callGraph) = processExpression (callG, exp1)
        
    CONST exp1 -> (callG)
    
    COND bexp1 exp1 exp2 -> (callGraph)  where
        (cg1, bexpress1) = processBexpression (callG, bexp1)
        (cg2, express1) = processExpression (cg1, exp1)
        (callGraph) =  processExpression (cg2, exp1)
  
  
processBexpression:: (CallGraph, BExp String String) -> (CallGraph)
processBexpression (callG, exp) = case (exp) of

    Lt exp1 exp2  -> (callGraph) where
        (cg1, expression1) = processExpression (callG, exp1)
        (callGraph, expression) = processExpression (callG, exp2)         
        
    Gt exp1 exp2  -> (callGraph) where
        cg1 = processExpression (callG, exp1)
        callGraph = processExpression (callG, exp2)
        
    Eq exp1 exp2  -> (callGraph) where
        (cg1, expression1) = processExpression (callG, exp1)
        (callGraph, expression) = processExpression (callG, exp2)
        
    OR exp1 exp2  -> (callGraph) where
        (cg1) = processBexpression (callG, exp1)
        (callGraph) = processBexpression (callG, exp2)
        
    AND exp1 exp2  -> (callGraph) where
            (cg1) = processBexpression (callG, exp1)
            (callGraph) = processBexpression (callG, exp2)
        
    NOT exp1 -> (callGraph) where
            (callGraph) = processBexpression (callG, exp1)                             
          
    TRUE          -> (callG)  
    FALSE         -> (callG)
 

updateCallGraph :: CallGraph -> String -> CallGraph
updateCallGraph [] name = error "Malformed Program"
updateCallGraph ((func,list):cg) name = case (name `elem` list) of 
    True -> ((func,list):cg)
    False -> ((func,name:list):cg)
    
addFuncToCallGraph :: CallGraph -> String -> CallGraph
addFuncToCallGraph [] name = (name, []):[]
addFuncToCallGraph graph name = case (name `elem` (map fst graph)) of 
    True -> graph
    False -> (name,[]):graph

letUpdate:: CallGraph -> [Fun String String] -> CallGraph
letUpdate ((nm,list):cg) functions = graph where

    names = listOfNames functions   
    graph = ((nm, names ++ list):cg)
   

processListOfExpressions:: (CallGraph, [Exp String String]) -> (CallGraph)
processListOfExpressions (callG, []) = (callG,[])
processListOfExpressions (callG, (e:exps)) = (callGraph) where
    (cg1) = processExpression (callG, e)
    (cg2) = processListOfExpressions (cg1,exps)
    (callGraph)= (cg2)
    
fun2 = (Prog 
             [Fun ("main",["x","y"],(ADD (VAR "x") (VAR "y")))
             ,Fun ("f",["x"], (LET 
                   [Fun ("g",["y"],ADD (VAR "y") (VAR "x"))
                   ,Fun ("h",["x","y"], ADD (APP "main" [VAR "x"])(VAR "y"))]
                     (ADD (APP "g" [VAR "x"])
                     (APP "h" [VAR "x"])) ))])
                     
listOfNames:: [Fun String String] -> [String]
listOfNames funcs = map (\(Fun (name,_,_)) -> name) funcs
