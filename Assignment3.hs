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

