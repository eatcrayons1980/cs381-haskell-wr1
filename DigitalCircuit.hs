{-
 -  Assigment 1
 -  Group: Hannah Adams, Paul Freeman, Marcos Zavala
 -}

module DigitalCircuit where

data Circuit = Cir Gates Links deriving Show
data Gates = Gate Int GateFn Gates
           | NoGate deriving Show
data GateFn = And | Or | Xor | Not deriving Show
data Links = FromTo (Int,Int) (Int, Int) Links
           | NoLink deriving Show

adder = Cir (Gate 1 Xor (Gate 2 And NoGate)) (FromTo (1,1) (2,1) (FromTo (1,2) (2,2) NoLink))

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

