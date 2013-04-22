module Arithmetic where

-- basic syntax
data Expr = N Int
          | Plus Expr Expr
          | Times Expr Expr
          | Neg Expr

-- alternative syntax
data Op = Add | Multiply | Negate deriving Show
data Exp = Num Int
         | Apply Op [Exp] deriving Show

-- expressions
expression = Apply Multiply [Apply Negate [Apply Add [Num 3, Num 4]], Num 7]

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

-- translators
transExpr :: Expr -> Exp
transExpr (N x) = Num x
transExpr (Plus x y) = Apply Add [transExpr x,transExpr y]
transExpr (Times x y) = Apply Multiply [transExpr x,transExpr y]
transExpr (Neg x) = Apply Negate [transExpr x]
