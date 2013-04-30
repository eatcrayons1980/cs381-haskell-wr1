{-
 -  Assigment 2
 -  Group: Hannah Adams, Paul Freeman, Marcos Zavala
 -}

module Assign2 where

import SVG (ppLines)
import Data.List (find)

{--------------------------------------------
 - Exercise 1
 --------------------------------------------}
type Prog1 = [Cmd1]

data Cmd1 = LD1 Int
		  | ADD1
		  | MULT1
		  | DUP1
		  deriving Show

type Stack1 = [Int]

type D1 = Maybe Stack1 -> Maybe Stack1

-- test programs
testA1 :: Prog1
testA1 = [LD1 3, DUP1, ADD1, DUP1, MULT1]

testB1 :: Prog1
testB1 = [LD1 3, ADD1]

testC1 :: Prog1
testC1 = []

-- semantic processor
sem1 :: Prog1 -> D1
sem1 []     c = c
sem1 (o:os) c = sem1 os (semCmd1 o c)

-- semantic commands
semCmd1 :: Cmd1 -> D1

semCmd1 (LD1 x) (Just xs) = Just (x:xs)
semCmd1 (LD1 _) _         = Nothing

semCmd1 ADD1 (Just (x:y:xs)) = Just (x+y:xs)
semCmd1 ADD1 _               = Nothing

semCmd1 MULT1 (Just (x:y:xs)) = Just (x*y:xs)
semCmd1 MULT1 _               = Nothing

semCmd1 DUP1 (Just (x:xs)) = Just (x:x:xs)
semCmd1 DUP1 _             = Nothing

-- program evaluator
eval1 :: Prog1 -> Maybe Stack1
eval1 l = sem1 l (Just [])



{--------------------------------------------
 - Exercise 2
 --------------------------------------------}
type Prog2 = [Cmd2]

data Cmd2 = LD2 Int
		  | ADD2
		  | MULT2
		  | DUP2
		  | DEF2 String Prog2
		  | CALL2 String
		  deriving Show

type Stack2 = [Int]

type Macros2 = [(String, Prog2)]

type D2 = Maybe (Stack2, Macros2) -> Maybe (Stack2, Macros2)

-- semantic processor
sem2 :: Prog2 -> D2
sem2 []     c = c
sem2 (o:os) c = sem2 os (semCmd2 o c)

-- semantic commands
semCmd2 :: Cmd2 -> D2

semCmd2 (LD2 x) (Just (xs, m)) = Just ((x:xs), m)
semCmd2 (LD2 _) _              = Nothing

semCmd2 ADD2 (Just ((x:y:xs), m)) = Just ((x+y:xs), m)
semCmd2 ADD2 _                    = Nothing

semCmd2 MULT2 (Just ((x:y:xs), m)) = Just ((x*y:xs), m)
semCmd2 MULT2 _                    = Nothing

semCmd2 DUP2 (Just ((x:xs), m)) = Just ((x:x:xs), m)
semCmd2 DUP2 _                  = Nothing

semCmd2 (DEF2 n p) (Just (l, ms)) = Just (l, (n,p):ms)
semCmd2 (DEF2 _ _) _              = Nothing

semCmd2 (CALL2 n) (Just (l, ms)) = case findDef n ms of
										Just x    -> sem2 (snd x) (Just (l, ms))
										otherwise -> Nothing
semCmd2 (CALL2 _) _              = Nothing

findDef :: String -> Macros2 -> Maybe (String, Prog2)
findDef n ms = find (\c -> fst c == n) ms

-- program evaluator
eval2 :: Prog2 -> Maybe (Stack2, Macros2)
eval2 l = sem2 l (Just ([], []))



{--------------------------------------------
 - Exercise 3
 --------------------------------------------}
data Cmd = Pen Mode
		 | MoveTo Int Int
		 | Seq Cmd Cmd deriving Show

data Mode = Up | Down deriving Show

type State = (Mode,Int,Int)

type Line = (Int,Int,Int,Int)
type Lines = [Line]

semS :: Cmd -> State -> (State,Lines)
semS (Pen m) (_, x, y)             = ((m, x, y), [])
semS (MoveTo x y) (Up, _, _)       = ((Up, x, y), [])
semS (MoveTo x2 y2) (Down, x1, y1) = ((Down, x2, y2), [(x1, y1, x2, y2)])
semS (Seq c1 c2) s0                = let (s1, as) = semS c1 s0 in
                                   		let (s2, bs) = semS c2 s1 in
									    	(s2, as++bs)

sem' :: Cmd -> Lines
sem' c = snd $ semS c (Up, 0, 0)

-- test functions to write Hannah's name
hannah_h :: Int -> Int -> Cmd
hannah_h x y = Seq (Pen Up) (Seq (MoveTo x y) (Seq (Pen Down) (Seq (MoveTo (x+0) (y+4)) (Seq (MoveTo (x+0) (y+2)) (Seq (MoveTo (x+2) (y+2)) (Seq (MoveTo (x+2) (y+4)) (MoveTo (x+2) (y+0))))))))

hannah_a :: Int -> Int -> Cmd
hannah_a x y = Seq (Pen Up) (Seq (MoveTo x y) (Seq (Pen Down) (Seq (MoveTo (x+0) (y+4)) (Seq (MoveTo (x+2) (y+4)) (Seq (MoveTo (x+2) (y+0)) (Seq (MoveTo (x+2) (y+2)) (MoveTo (x+0) (y+2))))))))

hannah_n :: Int -> Int -> Cmd
hannah_n x y = Seq (Pen Up) (Seq (MoveTo x y) (Seq (Pen Down) (Seq (MoveTo (x+0) (y+4)) (Seq (MoveTo (x+2) (y+0)) (MoveTo (x+2) (y+4))))))
