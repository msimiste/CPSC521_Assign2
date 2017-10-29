module TestFiles where


import AST
import AlphaRename

fun1 = (Prog [Fun ("main",["x","y","a","b"],(COND (Lt (VAR "a") (VAR "b")) (VAR "x") (VAR "y") ))])

fun2 = (Prog 
             [Fun ("main",["x","y"],(ADD (VAR "x") (VAR "y")))
             ,Fun ("f",["x"], (LET 
                   [Fun ("g",["y"],ADD (VAR "y") (VAR "x"))
                   ,Fun ("h",["x","y"], ADD (APP "main" [VAR "x"])(VAR "y"))]
                     (ADD (APP "g" [VAR "x"])
                     (APP "h" [VAR "x"])) ))])
                     
fun3 = (Prog [Fun ("main", ["x","y","z","n"], 
                (LET [Fun ("f1",["v"], (ADD (VAR "x")(APP "f2" [VAR "v"]))),
                Fun ("f2", ["j"], 
                (LET [Fun ("g2",["b"],(ADD (VAR "b") (APP "f3" [VAR "j"])))] 
                (ADD (APP "g2" [VAR "y"]) (APP "f3" [VAR "x"])))),
                Fun ("f3",["k"], 
                (LET [Fun ("g3", ["c"], (MUL (VAR "c") (APP "f1" [VAR "k"])))]
                 (APP "g3" [VAR "z"])))]
                      (APP "f1" [VAR "n"])))])                    
      
test3 = (Prog [Fun ("main",["x","y"],(ADD (VAR "x") (VAR "y")))
             ,Fun ("f",["z"], (LET 
                   [Fun ("g",["a"],MUL (VAR "z") (VAR "a"))
                   ,Fun ("h",["d","e"], DIV (VAR "d") (VAR "e"))]
                     (ADD (APP "h" [VAR "z"])
                     (APP "h" [VAR "z",CONST 7])) ))])
test5 = (Prog 
             [Fun ("main",["x","y"],(ADD (VAR "x") (VAR "y")))
             ,Fun ("f",["x"], (LET 
                   [Fun ("g",["y"],MUL (VAR "y") (VAR "x"))
                   ,Fun ("h",["x","y"], DIV (VAR "x") (VAR "y"))]
                     (ADD (APP "g" [VAR "x"])
                     (APP "h" [VAR "x",CONST 7])) ))])
                     
testTable = ([("y","X1"),("x","X0")],[("main","X9")],2,1)::ST
