module LearningHaskell.HW03 where

data Expression =
    Var String                   -- Variable
  | Val Int                      -- Integer literal
  | Op Expression Bop Expression -- Operation
  deriving (Show, Eq)

-- Binary (2-input) operators
data Bop =
    Plus
  | Minus
  | Times
  | Divide
  | Gt
  | Ge
  | Lt
  | Le
  | Eql
  deriving (Show, Eq)

data Statement =
    Assign   String     Expression
  | Incr     String
  | If       Expression Statement  Statement
  | While    Expression Statement
  | For      Statement  Expression Statement Statement
  | Sequence Statement  Statement
  | Skip
  deriving (Show, Eq)

type State = String -> Int

-- Exercise 1 -----------------------------------------
empty :: State
empty = const 0

extend :: State -> String -> Int -> State
extend state name value x = if x == name then value else state x

-- Exercise 2 -----------------------------------------

evalE :: State -> Expression -> Int
evalE _ (Val n) = n
evalE state (Var n) = state n
evalE state (Op exp1 op exp2)
  | op == Plus = value1 + value2
  | op == Minus = value1 - value2
  | op == Times = value1 * value2
  | op == Divide = if value2 == 0 then 0 else value1 `div` value2
  | op == Gt = boolAsInt (value1 > value2)
  | op == Ge = boolAsInt (value1 >= value2)
  | op == Lt = boolAsInt (value1 < value2)
  | op == Le = boolAsInt (value1 <= value2)
  | op == Eql = boolAsInt (value1 == value2)
  where boolAsInt bool = if bool then 1 else 0
        value1 = evalE state exp1
        value2 = evalE state exp2

-- Exercise 3 -----------------------------------------

data DietStatement = DAssign String Expression
                   | DIf Expression DietStatement DietStatement
                   | DWhile Expression DietStatement
                   | DSequence DietStatement DietStatement
                   | DSkip
                     deriving (Show, Eq)

desugar :: Statement -> DietStatement
desugar (Assign variable expression) = DAssign variable expression
desugar (Incr variable) = DAssign variable (Op (Var variable) Plus (Val 1))
desugar (If expression thenStmt elseStmt) = DIf expression (desugar thenStmt) (desugar elseStmt)
desugar (While expression stmt) = DWhile expression (desugar stmt)
desugar (Sequence stmt1 stmt2) = DSequence (desugar stmt1) (desugar stmt2)
desugar Skip = DSkip
desugar (For initialization loop update stmt) = DSequence
    (desugar initialization)
    (DWhile loop
        (DSequence
            (desugar stmt)
            (desugar update)))


-- Exercise 4 -----------------------------------------

evalSimple :: State -> DietStatement -> State
evalSimple state DSkip = state
evalSimple state (DAssign x expression) = extend state x (evalE state expression)
evalSimple state (DSequence stmt1 stmt2) = evalSimple (evalSimple state stmt1) stmt2
evalSimple state (DWhile expression stmt) = evalSimple state (DIf expression thenDo DSkip)
    where thenDo = DSequence stmt (DWhile expression stmt)
evalSimple state (DIf expression thenStmt elseStmt)
    | evalE state expression /= 0 = evalSimple state thenStmt
    | otherwise = evalSimple state elseStmt

run :: State -> Statement -> State
run = undefined

-- Programs -------------------------------------------

slist :: [Statement] -> Statement
slist [] = Skip
slist l  = foldr1 Sequence l

{- Calculate the factorial of the input

   for (Out := 1; In > 0; In := In - 1) {
     Out := In * Out
   }
-}
factorial :: Statement
factorial = For (Assign "Out" (Val 1))
                (Op (Var "In") Gt (Val 0))
                (Assign "In" (Op (Var "In") Minus (Val 1)))
                (Assign "Out" (Op (Var "In") Times (Var "Out")))


{- Calculate the floor of the square root of the input

   B := 0;
   while (A >= B * B) {
     B++
   };
   B := B - 1
-}
squareRoot :: Statement
squareRoot = slist [ Assign "B" (Val 0)
                   , While (Op (Var "A") Ge (Op (Var "B") Times (Var "B")))
                       (Incr "B")
                   , Assign "B" (Op (Var "B") Minus (Val 1))
                   ]

{- Calculate the nth Fibonacci number

   F0 := 1;
   F1 := 1;
   if (In == 0) {
     Out := F0
   } else {
     if (In == 1) {
       Out := F1
     } else {
       for (C := 2; C <= In; C++) {
         T  := F0 + F1;
         F0 := F1;
         F1 := T;
         Out := T
       }
     }
   }
-}
fibonacci :: Statement
fibonacci = slist [ Assign "F0" (Val 1)
                  , Assign "F1" (Val 1)
                  , If (Op (Var "In") Eql (Val 0))
                       (Assign "Out" (Var "F0"))
                       (If (Op (Var "In") Eql (Val 1))
                           (Assign "Out" (Var "F1"))
                           (For (Assign "C" (Val 2))
                                (Op (Var "C") Le (Var "In"))
                                (Incr "C")
                                (slist
                                 [ Assign "T" (Op (Var "F0") Plus (Var "F1"))
                                 , Assign "F0" (Var "F1")
                                 , Assign "F1" (Var "T")
                                 , Assign "Out" (Var "T")
                                 ])
                           )
                       )
                  ]