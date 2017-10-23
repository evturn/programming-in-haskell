data Expr = 
    Val Int 
  | Add Expr Expr

data Op =
    EVAL Expr
  | ADD Int

type Cont = [Op]

-- | A function that evaluates an expression in the context
-- of a control stack.
eval :: Expr -> Cont -> Int
-- | If the expression is an integer, it is already 
-- evaluated and we begin executing the control stack.
eval (Val n)   c = exec c n
-- | If the expression is an addition, we evaluate the
-- first argument, placing the operation `EVAL y` on
-- top of the control stack to indicate that the second
-- argument should be evaluated after the first argument.
eval (Add x y) c = eval x (EVAL y : c)

-- | A function that executes a control stack in the context
-- of an integer argument.
exec :: Cont -> Int -> Int
-- | If the control stack is empty, we return the integer.
exec []           n = n
-- | If the top of the stack is an operation `EVAL y` then
-- evaluate the expression, placing the operation `ADD n` on
-- top of the remaining stack to indicate that the current
-- integer argument should be added together with result of
-- `EVAL y`.
exec (EVAL y : c) n = eval y (ADD n : c)
-- | If the top of the stack is an operation `ADD n` then
-- evaluation of the two arguments of an addition expression
-- is complete and we execute the remaining control stack in
-- the context of the sum of the two resulting integer
-- values.
exec (ADD n : c)  m = exec c (n + m)

-- | A function that evaluates an expression to an integer
-- by invoking `eval` with the given expression and the
-- empty control stack.
value :: Expr -> Int
value e = eval e []
