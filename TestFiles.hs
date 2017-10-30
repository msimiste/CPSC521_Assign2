module TestFiles where


import AST
import AlphaRename

fun1 = (Prog [Fun ("f1",["x","y","a","b"],(ADD (APP "f1" [VAR "y"])(APP "f2" [VAR "t"])))])

fun6 = (Prog [Fun ("main", ["x","y","a","b"], (LET [Fun ("t1", ["a"], (ADD (VAR "a") (VAR "y")))] (SUB (VAR "x")(VAR "b"))))])
 
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

out = (Prog [Fun ("f0",["X0","X1","X2","X3"],
                LET [Fun ("f1",["X4"],ADD (VAR "X0") (APP "f2" [VAR "X4"])),
                Fun ("f2",["X5"],
                LET [Fun ("f4",["X6"],ADD (VAR "X6") (APP "f3" [VAR "X5"]))] 
                (ADD (APP "f4" [VAR "X1"]) (APP "f3" [VAR "X0"]))),Fun ("f3",["X7"],
                LET [Fun ("f5",["X8"],MUL (VAR "X8") (APP "f1" [VAR "X7"]))] (APP "f5" [VAR "X2"]))]
                (APP "f1" [VAR "X3"]))])

--symtab = 

--([("c","X8"),
--("k","X7"),
--("b","X6"),
--("j","X5"),
--("v","X4"),
--("n","X3"),
--("z","X2"),
--("y","X1"),
--("x","X0")],
--[("g3","f5"),
--("g2","f4"),
--("f3","f3"),
--("f2","f2"),
--("f1","f1"),
--("main","f0")],9,6)

      
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

cg = [("f5",["f1"]),("f3",["f5"]),("f4",["f3"]),("f2",["f3","f4"]),("f1",["f2"]),("f0",["f1"])]

ff = [("f5",[],[]),("f3",[],[]),("f4",[],[]),("f2",[],[]),("f1",[],[]),("f0",[],[])]

