module MiniLogo where

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

steps :: Int -> Cmd
steps n
    | n < 1     = Mult (Pen Up) (Mult (MoveTo (Num 0, Num 0)) (Pen Down))
    | otherwise = Mult (steps $ n-1) (Mult (MoveTo (Num $ n-1, Num n)) (MoveTo (Num n, Num n)))
