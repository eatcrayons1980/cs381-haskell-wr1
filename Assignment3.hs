{-
 -  Assigment 3
 -  Group: Hannah Adams, Paul Freeman, Marcos Zavala
 -}

module Assign3 where

import Data.List (find)

{--------------------------------------------
 - Exercise 1
 --------------------------------------------}
type Prog = [Cmd]

data Cmd = LD Int
		 | ADD
		 | MULT
		 | DUP
		 | INC
		 | SWAP
		 | POP Int
		 deriving Show

type Stack = [Int]

type D = Maybe Stack -> Maybe Stack

type Rank = Int
type CmdRank = (Rank, Rank)

-- semantic processor
sem :: Prog -> D
sem []     c = c
sem (o:os) c = sem os (semCmd o c)

-- semantic commands
semCmd :: Cmd -> D

semCmd (LD x) (Just xs) = Just (x:xs)
semCmd (LD _) _         = Nothing

semCmd ADD (Just (x:y:xs)) = Just (x+y:xs)
semCmd ADD _               = Nothing

semCmd MULT (Just (x:y:xs)) = Just (x*y:xs)
semCmd MULT _               = Nothing

semCmd DUP (Just (x:xs)) = Just (x:x:xs)
semCmd DUP _             = Nothing

semCmd INC (Just (x:xs)) = Just (x+1:xs)
semCmd INC _             = Nothing

semCmd SWAP (Just (x:y:xs)) = Just (y:x:xs)
semCmd SWAP _               = Nothing

semCmd (POP 0) (Just xs)     = Just xs
semCmd (POP k) (Just (x:xs)) = semCmd (POP (k-1)) (Just xs)
semCmd _ _                   = Nothing

rankC :: Cmd -> CmdRank
rankC (LD _)  = (0, 1)
rankC ADD     = (2, 1)
rankC MULT    = (2, 1)
rankC DUP     = (1, 2)
rankC INC     = (1, 1)
rankC SWAP    = (2, 2)
rankC (POP k) = (k, 0)

rankP :: Prog -> Maybe Rank
rankP cs = rank cs 0

rank :: Prog -> Rank -> Maybe Rank
rank [] r = Just r
rank (c:cs) r | pop <= r  = rank cs (r - pop + push)
			  | otherwise = Nothing
					where (pop, push) = rankC c

semStatTC :: Prog -> Maybe Stack
semStatTC p | rankP p == Nothing = Nothing
			| otherwise          = sem p (Just [])

{--------------------------------------------
 - Exercise 2
 --------------------------------------------}
data Shape = X
		   | TD Shape Shape
		   | LR Shape Shape
		   deriving Show

type BBox = (Int, Int)

bbox :: Shape -> BBox
bbox X = (1, 1)
bbox (TD t d) = (fst top + fst bot, max (snd top) (snd bot))
	where
		top = bbox t
		bot = bbox d
bbox (LR l r) = (max (fst left) (fst rght), snd left + snd rght)
	where
		left = bbox l
		rght = bbox r

rect :: Shape -> Maybe BBox
rect X = Just $ bbox X
rect (TD t d) | snd top == snd bot = Just (fst top + fst bot, snd top)
              | otherwise          = Nothing
	where
		top = bbox t
		bot = bbox d
rect (LR l r) | fst left == fst rght = Just (fst left, snd left + snd rght)
			  | otherwise		   = Nothing
	where
		left = bbox l
		rght = bbox r

{--------------------------------------------
 - Exercise 3
 --------------------------------------------}
f x y = if null x then [y] else x

g x y = if not (null x) then [] else [y]

{--------------------------------------------
 - 3.a.1
 -
 - The type of f is [t] -> t -> [t]
 -
 - The type of g is [a] -> a1 -> [a1]
 -
 - 3.a.2
 -
 - Since Haskell determines types at compile time,
 - the program ensures that the return type for 
 - f is the same regardless of the condition.
 - Therefore, since one the return options involves
 - x and one involves y, the two types are
 - associated together such that x is a list of y's. 
 -
 - For g, since the 'then' statement returns an
 - empty list, this is a valid case of list [y].
 - Therefore, Haskell assigns the return type to
 - be type [y]. This allows the x to be any other
 - type without affecting the output type of the
 - function.
 -
 - 3.a.3
 -
 - Function g is more general because it does
 - not force x to be a list of type y elements.
 - In function f, the types for x and y must
 - be the same type.
 -
 - 3.a.4
 -
 - As explained above, in function g, the empty
 - list is a valid element in the set of possible
 - lists of elements of type y. Therefore, the
 - return type is determined to be a list of y
 - and the type of x has no effect on the rest
 - of the function.
 -
 - 3.b                                          -}

h x y = head x : snd (head y) : []

{- 3.c                                          -}

k x y = x $ y x

{- 3.d
 -
 - A function (a -> b) cannot be created easily
 - due to the fact that a return type must be
 - specified during the definition of the function.
 - In this definition, the return type is either
 - the original type of the parameter, or a
 - transformation of that type. There is no simple
 - way to get back an unspecified type from a
 - function.
 -
 -}
