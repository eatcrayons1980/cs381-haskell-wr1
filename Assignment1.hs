{-
 -  Assigment 1
 -  Group: Hannah Adams, Paul Freeman, Marcos Zavala
 -}

{---------------------------------------------------
-- Part 1 - MiniLogo
----------------------------------------------------}
module Assignment where

data Cmd = Pen Mode
         | MoveTo (Pos, Pos)
         | Def String Pars Cmd
         | Call String Vals
         | Mult Cmd Cmd deriving Show

data Mode = Up | Down deriving Show
data Pos = Num Int | Name String deriving Show
data Pars = SingleP String
          | MultP String Pars deriving Show
data Vals = SingleV Int
          | MultV Int Vals deriving Show

-- def vector (x1,y1,x2,y2) pen up; moveto (x1,y1); pen down; moveto (x2,y2)

vector = Def "vector" (MultP "x1"
                      (MultP "y1"
                      (MultP "x2"
                      (SingleP "y2")))) (Mult (Pen Up)
                                        (Mult (MoveTo (Name "x1", Name "y1"))
                                        (Mult (Pen Down)
                                              (MoveTo (Name "x2", Name "y2")))))

-- Logo steps
steps :: Int -> Cmd
steps n
    | n < 1     = Mult (Pen Up) (Mult (MoveTo (Num 0, Num 0)) (Pen Down))
    | otherwise = Mult (steps $ n-1) (Mult (MoveTo (Num $ n-1, Num n)) (MoveTo (Num n, Num n)))



{---------------------------------------------------
-- Part 2 - Digital Circuit
----------------------------------------------------}

data Circuit = Cir Gates Links deriving Show
data Gates = Gate Int GateFn Gates
           | NoGate deriving Show
data GateFn = And | Or | Xor | Not deriving Show
data Links = FromTo (Int,Int) (Int, Int) Links
           | NoLink deriving Show

-- adder
adder = Cir (Gate 1 Xor (Gate 2 And NoGate)) (FromTo (1,1) (2,1) (FromTo (1,2) (2,2) NoLink))

-- pretty printer
ppCirc :: Circuit -> String
ppCirc (Cir gates links) = ppGate gates ++ ppLink links

ppGate (Gate x gatefn gates) = show x ++ ":" ++ ppGateFn gatefn  ++ ppGate gates
ppGate NoGate = []

ppGateFn And = "and;\n"
ppGateFn Or  = "or;\n"
ppGateFn Xor = "xor;\n"
ppGateFn Not = "not;\n"

ppLink (FromTo (a,b) (c,d) links) = "from " ++ show a ++ "." ++ show b ++ " to " ++ show c ++ "." ++ show d ++ ";\n" ++ ppLink links
ppLink NoLink = []



{---------------------------------------------------
-- Part 3 - Arithmetic
----------------------------------------------------}

-- basic syntax
data Expr = N Int
          | Plus Expr Expr
          | Times Expr Expr
          | Neg Expr

-- alternative syntax
data Op = Add | Multiply | Negate deriving Show
data Exp = Nu Int
         | Apply Op [Exp] deriving Show

-- expressions
expression = Apply Multiply [Apply Negate [Apply Add [Nu 3, Nu 4]], Nu 7]

expression' = Times (Neg (Plus (N 3) (N 4))) (N 7)

{-
 - Exercise 3, Part B
 -
 - An advantage of the alternative syntax is that by having the operators
 - their own separate data type they can be reused more easily.
 -
 - A disadvantage is that it requires more than one line and also separates
 - out the functionality into two locations, making it harder to interpret.
 -
 - 
 -
 -}

-- translator
transExpr :: Expr -> Exp
transExpr (N x) = Nu x
transExpr (Plus x y) = Apply Add [transExpr x,transExpr y]
transExpr (Times x y) = Apply Multiply [transExpr x,transExpr y]
transExpr (Neg x) = Apply Negate [transExpr x]
