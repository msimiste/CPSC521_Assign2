import ParseProg
import AST

data Mexp a b = BExp a b | Exp a b deriving Show


type AItem = (String, Int)

--data AList a = Alist ([AItem],Int) deriving Show
type AList = ([AItem],Int)
    
varList:: (Printer b) => (Prog a b) -> Int -> AList 
varList (Prog a) num = ourList where   
        --(ourList1, num1) = foldr(\(Fun (name,list,exps)) (acc,x) -> parseExp exps x) ([],num) funcs 
        (ourList2, num2) = foldr(\func (acc,y) ->  parseFun func y (acc,y))  ([],num) funcs --foldr(\func (acc,y) ->  parseFun func y)  ([],num1) funcs 
        ourList = (ourList2, num2)--(ourList1 ++ ourList2, num2)
        funcs = funList (Prog a)

funList:: (Prog a b) -> [Fun a b]
funList (Prog funcs) = funcs


parseFun::(Printer b) => (Fun a b) -> Int -> AList -> AList
parseFun (Fun a) num aList = case (Fun a) of
    Fun (name, args, exp) -> list where
        (flist, num1) = parseFuncArgs args num 
        (slist, num2) = parseExp exp num1 aList
        list = (flist ++ slist, num2)

parseFuncArgs::(Printer b) =>  [b] -> Int -> AList
parseFuncArgs [] num = ([], num)
parseFuncArgs (s:str) num = (aItems, number) where
    (aItems,number) = ([(parseAItem s num)] ++ fst (parseFuncArgs str (num+1)),num+2)     

parseAItem:: (Printer b) =>  b -> Int -> AItem
parseAItem s num = (printer s, num)


parseExp:: (Printer b) => (Exp a b) -> Int -> AList -> AList
parseExp exp num (aItems,aNum) = case exp of
    VAR exp -> ([parseAItem exp num] ++ aItems,num+1)
    ADD exp1 exp2 -> list where
            (flist, num1) = parseExp exp1 num (aItems, aNum)
            (slist, num2) = parseExp exp2 num1 (aItems, num1)
            list = (flist ++ slist ++ aItems,num2)
    SUB exp1 exp2 -> list where    
            (flist, num1) = parseExp exp1 num (aItems, aNum)
            (slist, num2) = parseExp exp2 num1 (aItems, num1)
            list = (flist ++ slist ++ aItems,num2)      
    MUL exp1 exp2 -> list where    
            (flist, num1) = parseExp exp1 num (aItems, aNum)
            (slist, num2) = parseExp exp2 num1 (aItems, num1)
            list = (flist ++ slist ++ aItems,num2)
    DIV exp1 exp2 -> list where    
            (flist, num1) = parseExp exp1 num (aItems, aNum)
            (slist, num2) = parseExp exp2 num1 (aItems, num1)
            list = (flist ++ slist ++ aItems,num2)
    NEG exp1 -> parseExp exp1 num (aItems, aNum)
    CONST exp -> ([parseAItem "V" exp], num)
    COND bexp1 exp1 exp2 -> list where
            (flist, num1) = parseBexp bexp1 num (aItems, aNum)
            (slist, num2) = parseExp exp1 num1 (aItems, num1)
            (tlist, num3) = parseExp exp2 num2 (aItems, num2)
            list = (flist ++ slist ++ tlist ++ aItems, num3)
    APP exp exps -> foldr(\exps (acc,y) -> parseExp exps y (acc,y)) (aItems,aNum) exps
    LET funcs exp2 -> list where
        (flist, num1) = foldr(\func (acc,y) -> parseFun func y (acc,y)) (aItems,aNum) funcs
        (slist, num2) = foldr(\func (acc,y) ->  parseFun func y(acc,y))  (aItems,num1) funcs
        (tlist, num3) = parseExp exp2 num2 (aItems, num2)
        list = (flist ++ slist ++ tlist ++ aItems, num3)
     

parseBexp:: (Printer b) => (BExp a b) -> Int -> AList -> AList
parseBexp exp num (aItems, aNum) = case exp of
    Lt exp1 exp2 -> list where
        (flist, num1) = parseExp exp1 num (aItems, aNum)
        (slist, num2) = parseExp exp2 num1 (aItems, num1)
        list = (flist ++ slist ++ aItems,num2)
    Gt exp1 exp2 -> list where
        (flist, num1) = parseExp exp1 num (aItems, aNum)
        (slist, num2) = parseExp exp2 num1 (aItems, num1)
        list = (flist ++ slist ++ aItems,num2)
    Eq exp1 exp2 -> list where
        (flist, num1) = parseExp exp1 num (aItems, aNum)
        (slist, num2) = parseExp exp2 num1 (aItems, aNum)
        list = (flist ++ slist ++ aItems,num2)
    AND bexp1 bexp2 -> list where
        (flist, num1) = parseBexp bexp1 num (aItems, aNum)
        (slist, num2) = parseBexp bexp2 num1 (aItems, aNum)
        list = (flist ++ slist ++ aItems, num2)
    OR  bexp1 bexp2 -> list where
        (flist, num1) = parseBexp bexp1 num (aItems, aNum)
        (slist, num2) = parseBexp bexp2 num1 (aItems, aNum)
        list = (flist ++ slist ++ aItems, num2)
    NOT bexp1 -> parseBexp bexp1 num (aItems, aNum)
    
    _ -> ([], num)
    


fun1 = (Prog [Fun ("main",["x","y"],(SUB (VAR "x") (VAR "y")))])

--fun2 = (Prog [(Fun ("main",["x","y","z"], LET [Fun ("f", ["y"], ADD (VAR "x") (APP "g" (VAR "y")))])
 --       , [])])
         
test3 = (Prog [Fun ("main",[],(ADD (VAR "x") (VAR "y")))
             ,Fun ("f",["x"], (LET 
                   [Fun ("g",["y"],MUL (VAR "y") (VAR "x"))
                   ,Fun ("h",["x","y"], DIV (VAR "x") (VAR "y"))]
                     (ADD (APP "g" [VAR "x"])
                     (APP "h" [VAR "x",CONST 7])) ))])

