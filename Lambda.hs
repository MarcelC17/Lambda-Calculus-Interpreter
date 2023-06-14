module Lambda where

import Expr
import Data.List
import Data.List (nub)
-- TODO 1.1. find free variables of a Expr

free_vars :: Expr -> [String]
free_vars (Variable var) = [var]
free_vars (Function var expression) = nub $ removeVar var (free_vars expression)
free_vars (Application expression1 expression2) = nub $ free_vars expression1 ++ free_vars expression2

removeVar :: Eq a => a -> [a] -> [a]
removeVar _ [] = []
removeVar x (y:ys)
  | x == y = removeVar x ys
  | otherwise = y : removeVar x ys


-- TODO 1.2. reduce a redex
contains :: [String] -> String -> Bool
contains list var = elem var list

replace :: Expr -> String -> Expr
replace (Variable var) toReplace = if var == toReplace then Variable "a" else Variable var
replace (Function var exp) toReplace = if var == toReplace then Function "a" (replace exp toReplace) else Function var (replace exp toReplace);
replace (Application exp1 exp2) toReplace = Application (replace exp1 toReplace) (replace exp2 toReplace)

reduce :: Expr -> String -> Expr -> Expr
reduce (Variable var1) var2 newexp = if var1 == var2 then newexp else Variable var1
reduce (Function var1 exp) var2 newexp = if var1 == var2 then Function var1 exp
                        else if contains (free_vars newexp) var1 then Function "a" (reduce (replace exp var1) var2 newexp)
                        else Function var1 (reduce exp var2 newexp)
                        
reduce (Application exp1 exp2) var2 newexp = Application (reduce exp1 var2 newexp) (reduce exp2 var2 newexp)

-- Normal Evaluation
-- TODO 1.3. perform one step of Normal Evaluation
stepN :: Expr -> Expr
stepN (Variable var) = Variable var
stepN (Function var exp) =  Function var (stepN exp)
stepN (Application (Function var exp1) exp2) = reduce exp1 var exp2
stepN (Application (Variable var) exp) = Application (Variable var) (stepN exp)
stepN (Application exp1 exp2) = Application (stepN exp1) exp2

-- TODO 1.4. perform Normal Evaluation
reduceN :: Expr -> Expr
reduceN exp
  | reduced == exp = exp
  | otherwise = reduceN reduced
  where
    reduced = stepN exp

reduceAllN :: Expr -> [Expr]
reduceAllN exp 
    | reduced == exp = [exp]
    | otherwise = exp : reduceAllN reduced
    where
        reduced = stepN exp

-- Applicative Evaluation
-- TODO 1.5. perform one step of Applicative Evaluation
stepA :: Expr -> Expr
stepA (Variable var)= Variable var
stepA (Function var exp) =  Function var (stepA exp)
stepA (Application (Function var exp1) exp2) = if (reducible exp1) then (Application (Function var (stepA exp1)) exp2)
                                                else if (reducible exp2) then (Application (Function var exp1) (stepA exp2)) 
                                                else reduce exp1 var exp2
stepA (Application exp1 exp2) = if (reducible exp1) then Application (stepA exp1) exp2
                                else Application exp1 (stepA exp2)


reducible :: Expr -> Bool
reducible (Variable var) = False
reducible (Function var expr) =  reducible expr
reducible (Application (Function var exp1) exp2) = True
reducible (Application exp1 exp2) = if (reducible exp1) then True
                        else if (reducible exp2) then True
                        else False

-- TODO 1.6. perform Applicative Evaluation
reduceA :: Expr -> Expr
reduceA exp
  | reduced == exp = exp
  | otherwise = reduceA reduced
  where
    reduced = stepA exp

reduceAllA :: Expr -> [Expr]
reduceAllA  exp 
    | reduced == exp = [exp]
    | otherwise = exp : reduceAllA reduced
    where
        reduced = stepA exp

-- TODO 3.1. make substitutions into a expression with Macros
evalMacros :: [(String, Expr)] -> Expr -> Expr
evalMacros = undefined

-- TODO 4.1. evaluate code sequence using given strategy
evalCode :: (Expr -> Expr) -> [Code] -> [Expr]
evalCode = undefined
