import ParseProg
import AST

data Mexp a b = BExp a b | Exp a b deriving Show


--varList:: Mexpression -> [String]
--varList m = case m of 
--    Exp a b 


--varList:: Fun a b -> ([String], [String])
--varList  Fun (a, b, c) = 

type AItem = (String, Int)

--data AList a = Alist ([AItem],Int) deriving Show
type AList = ([AItem],Int)
    
varList:: (Printer b) => (Prog a b) -> Int -> AList 
varList (Prog a) num = ourList where   
        (ourList1, num1) = foldr(\(Fun (name,list,exps)) (acc,x) -> parseExp exps x) ([],num) funcs  where
        (ourList2, num2) = foldr(\func (acc,y) ->  parseFun func y)  ([],num1)  funcs where
        ourList = (ourList1 ++ ourList2, num1)
        funcs = funList (Prog a)

funList:: (Prog a b) -> [Fun a b]
funList (Prog funcs) = funcs


parseFun::(Printer b) => (Fun a b) -> Int -> AList 
parseFun (Fun a) num = case (Fun a) of
    Fun (name, vars, exp) -> parseFuncVars vars num 

parseFuncVars::(Printer b) =>  [b] -> Int -> AList 
parseFuncVars []  num = ([], num)
parseFuncVars (s:str) num = (aItems, number) where
    (aItems,number) = ([(parseAItem s num)] ++ fst (parseFuncVars str (num+1)),num+2)
     
--((map (\x -> parseAItem x (num+1)) str),num)
--((foldr(\x y -> map parseItem y x) num str),)
--((s,num)):(parseVars str num+1)


parseAItem:: (Printer b) =>  b -> Int -> AItem
parseAItem s num = (printer s, num)
--((((printer s),num):(parseVars str (num+1))),num)

parseExp:: (Printer b) => (Exp a b) -> Int -> AList
parseExp exp num = case exp of
    exp1 -> case exp1 of
        VAR  exp -> ([parseAItem exp num],num+1)
        ADD  exp2 exp3 -> list where
            (flist, num1) = parseExp exp2 num
            (slist, num2) = parseExp exp3 num1
            list = (flist ++ slist,num2)
        --SUB  exp1 exp2 -> item where
        --DIV  exp1 exp2 -> item where
        --NEG  exp1 exp2 -> item where
        --    (
        --CONST num
        --COND bexp exp1 exp2 -> ([],num)
        --APP  a exps
        --LET  funcs exp
--parseVars:: (Printer b) => (Mexp a b) -> Int -> AList
--parseVars exp num = case exp of
--    (BExp a)  -> case (BExp a) of
--        TRUE -> ([],num)
--        FALSE -> ([],num)
--        NOT (BExp exp1 exp2) -> parseVars (BExp exp1 exp2) num
--        Lt (Exp exp1 exp2) (Exp exp3 exp4) -> thisList where
--            (fList, num1) = parseVars (Exp exp1 exp2) num
--            (sList, num2) = parseVars (Exp exp3 exp4) num1
--            thisList = (fList ++ sList, num2)
--        Eq (Exp exp1 exp2) (Exp exp3 exp4) -> thisList where
--            (fList, num1) = parseVars (Exp exp1 exp2) num
--            (sList, num2) = parseVars (Exp exp3 exp4) num1
--            thisList = (fList ++ sList, num2)
--        AND (Exp exp1 exp2) (Exp exp3 exp4) -> thisList where
--            (fList, num1) = parseVars (Exp exp1 exp2) num
--            (sList, num2) = parseVars (Exp exp3 exp4) num1
--            thisList = (fList ++ sList, num2)
--        OR (Exp exp1 exp2) (Exp exp3 exp4) -> thisList where
--            (fList, num1) = parseVars (Exp exp1 exp2) num
--            (sList, num2) = parseVars (Exp exp3 exp4) num1
--            thisList = (fList ++ sList, num2)       
--    (Exp a b) -> case (Exp a b) of
--        VAR b -> parseAItem b num
--        ADD (Exp exp1 exp2) (Exp exp3 exp4) -> expList where
--            (fList2, num1) = parseVars (Exp exp1 exp2) num
--            (sList2, num2) = parseVars (Exp exp3 exp4) num1
--            expList = (fList2 ++ sList2, num2)
--        SUB(Exp exp1 exp2) (Exp exp3 exp4) -> expList where
--            (fList2, num1) = parseVars (Exp exp1 exp2) num
--            (sList2, num2) = parseVars (Exp exp3 exp4) num1
--            expList = (fList2 ++ sList2, num2)
--        MUL (Exp exp1 exp2) (Exp exp3 exp4) -> expList where
--            (fList2, num1) = parseVars (Exp exp1 exp2) num
--            (sList2, num2) = parseVars (Exp exp3 exp4) num1
--            expList = (fList2 ++ sList2, num2)
--        DIV (Exp exp1 exp2) (Exp exp3 exp4) -> expList where
--            (fList2, num1) = parseVars (Exp exp1 exp2) num
--            (sList2, num2) = parseVars (Exp exp3 exp4) num1
--            expList = (fList2 ++ sList2, num2)
--        NEG (Exp exp1 exp2) -> parseVars (Exp exp1 exp2) num
--        _ -> ([],num)
    


fun1 = (Prog [Fun ("main",["x","y"],(ADD (VAR "x") (VAR "y")))])
          
