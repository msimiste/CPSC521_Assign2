
import ParseProg
import AST
import Data.List
import Data.Maybe




type AItem = (String, Int)

--data AList a = Alist ([AItem],Int) deriving Show
type AList = ([AItem],Int)

type ST = ([AItem],[AItem], Int, Int)


---newtype State s a = State { runState :: s -> (ST, Fun String String) }
---
---instance Monad State ST Fun String String where
---
---st >>= f = State (\s -> let (s1,x) = app st s in app (f x) st1)



varList::  (Prog String String) -> [Fun String String]--[Fun String String]--ST
varList (Prog prog) = functions where 
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
renameFunction  (table, fun) = (st,(Fun (name,args,exp))) where
    (st1, (Fun (name,_,_))) = replaceFuncName (table, fun)
    (st2,(Fun (_,args,_))) = replaceFuncArgs (st1, fun)
    (st, (Fun (_,_,exp))) = replaceFuncExp (st2,fun)        
 

replaceFuncName:: (ST,(Fun String String)) -> (ST,(Fun String String))
replaceFuncName ((vList,fList,vNum,fNum),(Fun (name, args,exp))) = case (checkInList name fList) of 
    True -> ((vList, fList, vNum, fNum),retFunction) where
        index = getIndex name fList
        (str, num) = fList !! index
        retFunction = (Fun ("f" ++ show num,args,exp)) 
    False -> ((vList,(name,fNum):fList, vNum, (fNum+1)),retFunction) where
        retFunction = (Fun ("f" ++ show fNum,args,exp))
        
replaceFuncArgs:: (ST,(Fun String String)) -> (ST, (Fun String String))
replaceFuncArgs (symTable,(Fun (name,args,exp))) = (table, (Fun (name, arguments, exp))) where
    (table, arguments) = replaceFuncArg symTable args


replaceFuncArg:: ST -> [String] -> (ST, [String])
replaceFuncArg tble [] = (tble, [])
replaceFuncArg (vList,fList,vNum,fNum) (arg:args) = (symTable, arguments) where
    (t1,a) = case (checkInList arg vList) of
        True -> ((vList,fList,vNum,fNum), renameA) where
                index = getIndex arg vList
                (str, num) = vList !! index
                renameA = "X" ++ show num        
        False -> (((arg,vNum):vList, fList,(vNum+1), fNum), num1) where
            num1 = "X" ++ show vNum
    (t2,args1) = replaceFuncArg t1 args
    (symTable,arguments) = (t2,a:args1)

replaceFuncExp:: (ST,Fun String String) -> (ST,Fun String String)
replaceFuncExp (table, (Fun (n,a,exp))) = (table, (Fun (n,a,expression))) where
    expression = replaceExpName (table,exp)


--case exp of
--    (VAR exp) -> case (checkInList  exp vList) of 
--        True -> ((vList, fList, vNum, fNum), exp1) where
--            index = (getIndex exp vList)
--            (str,num) = vList !! index
--            funList1 = funList
--            exp1 = ("X" ++ show num)
--        False -> (((exp,vNum):vList, fList, (vNum+1), fNum), VAR exp1) where
--            exp1 = ("X" ++ show vNuum)   

      
replaceExpName:: (ST, (Exp String String)) -> (Exp String String)--(ST, (Exp String String))
replaceExpName ((vList, fList, vNum, fNum), exp) = case exp of
    (VAR exp) -> case (checkInList exp vList) of 
        True -> VAR exp1 where--((vList, fList, vNum, fNum), VAR exp1) where
            index = (getIndex exp vList)
            (str,num) = vList !! index
            funList1 = funList
            exp1 = ("X" ++ show num)
        False -> error $ "No Argument for variable: VAR " ++ exp--((((show exp,vNum):vList),fList,(vNum+1),fNum), VAR exp2) where
            --exp2 = ("X" ++ show vNum)
    ADD exp1 exp2 -> ADD express1 express2 where--(tbl, expression) where
        express1 = replaceExpName ((vList, fList, vNum, fNum), exp1)
        express2 = replaceExpName ((vList, fList, vNum, fNum), exp2)
        --(tbl,expression) = (t2, ADD express1 express2) 
    SUB exp1 exp2 -> SUB express1 express2 where --(tbl, expression) where
        express1 = replaceExpName ((vList, fList, vNum, fNum), exp1)
        express2 = replaceExpName ((vList, fList, vNum, fNum), exp2)
        --(tbl,expression) = (t2, SUB express1 express2) 
    MUL exp1 exp2 -> MUL express1 express2 where --(tbl, expression) where
        express1 = replaceExpName ((vList, fList, vNum, fNum), exp1)
        express2 = replaceExpName ((vList, fList, vNum, fNum), exp2)
        --(tbl,expression) = (t2, MUL express1 express2) 
    DIV exp1 exp2 -> DIV express1 express2 where --(tbl, expression) where
        express1 = replaceExpName ((vList, fList, vNum, fNum), exp1)
        express2 = replaceExpName ((vList, fList, vNum, fNum), exp2)
        --(tbl,expression) = (t2, DIV express1 express2) 
    NEG exp1 -> NEG express1 where--(tbl, expression) where
        express1 = replaceExpName ((vList, fList, vNum, fNum), exp1)
        --(tbl, expression) = (t1, NEG ex)
    CONST exp1 -> CONST exp1 --where
        --(t1, express1) = replace((("",exp):vList, fList, vNum, fNum), exp)      
    COND bexp1 exp1 exp2 -> COND bexpress1 express1 express2 where
        bexpress1 = replaceBexpName ((vList, fList, vNum, fNum), bexp1)
        express1 = replaceExpName ((vList, fList, vNum, fNum), exp1)
        express2 = replaceExpName ((vList, fList, vNum, fNum), exp2)
            
    LET funcs exp2 -> LET funcs2 expression where
        (t1, funcs1) = letFunctionP1 ((vList, fList, vNum, fNum), funcs)
        funcs2 = map (\func ->  renameLetFunction (t1, func)) funcs1
        --(t1, funcs1) = goFun ((vList, fList, vNum, fNum), funcs)
        expression = replaceExpName (t1, exp2)
        
      
    APP exp1 exps -> APP expression1 expressions where--(tbl, expression) where
        expression1 = replaceAppString ((vList, fList, vNum, fNum), exp1)
        expressions = map (\expr -> replaceExpName ((vList, fList, vNum, fNum), expr)) exps
    ---(tbl,list) where
    ---    (symTable,funs1) = rename_funName exp (symTable,funList)
    ---    (tbl, funs2) = foldl(\(sTable,funcs) exps -> parseExp exps (sTable,funcs)) (symTable,funs1) exps
    ---    list = funs2 --fix app of fcn name
  
    
    --(tbl,list) where
    --    (symTable1,funs1) =  foldl(\(sTable,funs) func -> parseFun func (sTable,funs)) (symTable,funList) funcs  
    --    (tbl,funs2) = parseExp exp2 (symTable1,funs1)
    --    list = funs2

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
    True -> str2 where
        index = getIndex str fList
        (str1, num) = fList !! index
        str2 = ("f" ++ show num)
    False -> error $ "No Argument for variable: VAR " ++ str
    
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
    
   
checkInList:: String -> [AItem] -> Bool
checkInList item list = (item `elem` (map fst list))
 
getIndex:: String -> [AItem] -> Int
getIndex item list = fromJust (elemIndex item (map fst list))
 
 
fun1 = (Prog [Fun ("main",["x","y","a","b"],(COND (Lt (VAR "a") (VAR "b")) (VAR "x") (VAR "y") ))])
      
test3 = (Prog [Fun ("main",["x","y"],(ADD (VAR "x") (VAR "y")))
             ,Fun ("f",["z"], (LET 
                   [Fun ("g",["a"],MUL (VAR "z") (VAR "a"))
                   ,Fun ("h",["d","e"], DIV (VAR "d") (VAR "a"))]
                     (ADD (APP "h" [VAR "e"])
                     (APP "h" [VAR "e",CONST 7])) ))])
test5 = (Prog 
             [Fun ("main",["x","y"],(ADD (VAR "x") (VAR "y")))
             ,Fun ("f",["x"], (LET 
                   [Fun ("g",["y"],MUL (VAR "y") (VAR "x"))
                   ,Fun ("h",["x","y"], DIV (VAR "x") (VAR "y"))]
                     (ADD (APP "g" [VAR "x"])
                     (APP "h" [VAR "x",CONST 7])) ))])

testTable = ([("y",1),("x",0)],[("main",9)],2,1)::ST
