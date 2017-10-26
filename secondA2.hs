
import ParseProg
import AST
import Data.List
import Data.Maybe -- prove this
import Data.String



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
        --(ourList,functions) = foldl(\(sTable,funcs1) func ->  renameFunction func (sTable,funcs1) )  (([],[],0,0),funcs) funcs 
        (table, functions) = goFun (([],[],0,0),funcs)
        funcs = funList (Prog prog)

funList:: (Prog String String) -> [Fun String String]
funList (Prog funcs) = funcs

goFun:: (ST, [Fun String String]) ->(ST, [Fun String String])
goFun (table, []) = (table,[])
goFun (table, (f:fs)) = (tbl, fcns) where
    (st1, fcn) = renameFunction f (table, (f:fs))
    (st2, fss) = goFun (st1,fs)
    (tbl,fcns) = (st2,fcn:fss)

  
renameFunction:: (Fun String String) -> (ST, [Fun String String]) -> (ST, Fun String String)
renameFunction  (Fun(nm, args,exp)) (table, funs) = (st,(Fun (name,args,exp))) where
    (st, (Fun (name,_,_))) = replaceFuncName (table, (Fun(nm, args,exp)))
    --(st2,(Fun (_,args,_))) = replaceFuncArgs (st1, fun)
   -- exp = replaceFuncExp (st2,fun)
    
        

--myMap::((ST, Fun String String) -> (ST, Fun String String)) -> (ST, [Fun String String]) -> (ST, [Fun String String])
--myMap _ (tble,[]) = (tble,[])
--myMap f (tbl,f:fcns) = (table, funcs) where
--(f tbl f):
--foldl(\(tble,funs) fun -> f (tble,fun))(tbl,[]) (lst)   

replaceFuncName:: (ST,(Fun String String)) -> (ST,(Fun String String))
replaceFuncName ((vList,fList,vNum,fNum),(Fun (name, args,exp))) = case (checkInList name fList) of 
    True -> ((vList, fList, vNum, fNum),retFunction) where
        index = getIndex name fList
        (str, num) = fList !! index
        retFunction = (Fun ("f" ++ show num,args,exp)) 
    False -> ((vList,(name,fNum):fList, vNum, (fNum+1)),retFunction) where
        retFunction = (Fun ("f" ++ show fNum,args,exp))
        
replaceFuncArgs:: (ST,(Fun String String)) -> (ST, (Fun String String))
replaceFuncArgs (symTable,(Fun (name,args,exp))) = (tbl,fun) where
    (tbl,args2) = foldl(\(tbl,args1) arg -> (tbl,(snd(replaceFuncArg tbl arg)):args1))(symTable,[]) args
    fun = (Fun (name, args2, exp))


replaceFuncArg:: ST -> String -> (ST, String)
replaceFuncArg str [] = (str, [])
replaceFuncArg (vList,fList,vNum,fNum) arg = case (checkInList arg vList) of
    True -> ((vList,fList,vNum,fNum), renameA) where
            index = getIndex arg fList
            (str, num) = fList !! index
            renameA = "X" ++ show num        
    False -> (((arg,vNum):vList, fList,(vNum+1), fNum), num1) where
        num1 = "X" ++ show vNum

replaceFuncExp:: (ST,Fun String String) -> (ST,Fun String String)
replaceFuncExp blah = blah        
---unPackTuple:: [(a,b)] -> (a,[b])
---unPackTuple [] = ()
---unPackTuple list = ((fst(last(list))),(map snd list))
---         
replaceExpName:: (ST, (Exp String String)) -> (ST, (Exp String String))
replaceExpName ((vList, fList, vNum, fNum), exp) = case exp of
    (VAR exp) -> case (checkInList  exp vList) of 
        True -> ((vList, fList, vNum, fNum), VAR exp1) where
            index = (getIndex exp vList)
            (str,num) = vList !! index
            funList1 = funList
            exp1 = ("X" ++ show num)
        False -> (((exp,vNum):vList, fList, (vNum+1), fNum), VAR exp1) where
            exp1 = ("X" ++ show vNum)
    
checkInList:: String -> [AItem] -> Bool
checkInList item list = (item `elem` (map fst list))
 
getIndex:: String -> [AItem] -> Int
getIndex item list = fromJust (elemIndex item (map fst list))
 
 
fun1 = (Prog [Fun ("main",["x","y"],(ADD (VAR "x") (VAR "y")))])
      
test3 = (Prog [Fun ("main",[],(ADD (VAR "x") (VAR "y")))
             ,Fun ("f",["z"], (LET 
                   [Fun ("g",["a"],MUL (VAR "b") (VAR "c"))
                   ,Fun ("h",["d","e"], DIV (VAR "m") (VAR "j"))]
                     (ADD (APP "t" [VAR "k"])
                     (APP "h" [VAR "l",CONST 7])) ))])
test5 = (Prog 
             [Fun ("main",[],(ADD (VAR "x") (VAR "y")))
             ,Fun ("f",["x"], (LET 
                   [Fun ("g",["y"],MUL (VAR "y") (VAR "x"))
                   ,Fun ("h",["x","y"], DIV (VAR "x") (VAR "y"))]
                     (ADD (APP "g" [VAR "x"])
                     (APP "h" [VAR "x",CONST 7])) ))])

testTable = ([("y",1),("x",0)],[("main",9)],2,1)::ST
