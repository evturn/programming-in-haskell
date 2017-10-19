data Prop = Const Bool
          | Var Char
          | Not Prop
          | And Prop Prop
          | Imply Prop Prop

type Assoc k v = [(k, v)]

type Subst = Assoc Char Bool

find :: Eq k => k -> Assoc k v -> v
find k t = head [v | (k', v) <- t, k == k']

-- | A function that evaluates a proposition given a substitution for its variables
-- defined by pattern matching on the five possible forms that the proposition can
-- have.
eval :: Subst -> Prop -> Bool
-- | The value of a constant proposition is simply the constant itself.
eval _ (Const b)   = b
-- | The value of a variable is obtained by looking up its value in the 
-- substitution.
eval s (Var x)     = find x s
-- | The value of a conjunction is given by taking the conjunction of the values of
-- the two argument propositions.
eval s (Not p)     = not (eval s p)
eval s (And p q)   = eval s p && eval s q
-- | The value of an implication is obtained by the `<=` ordering on logical values.
eval s (Imply p q) = eval s p <= eval s q

-- | A function that returns a list of all the variables in a proposition.
vars :: Prop -> [Char]
vars (Const _)   = []
vars (Var x)     = [x]
vars (Not p)     = vars p
vars (And p q)   = vars p ++ vars q
vars (Imply p q) = vars p ++ vars q

-- | A function that produces a list of logical values of a given length.
bools :: Int -> [[Bool]]
bools 0 = [[]]
-- | Append the results of taking two copies of the recursively produced lists
bools n = nah bss ++ yes bss
  where 
    -- | Place `False` in front of each list in the first copy.
    nah = map (False:)
    -- | Place `True` in front of each list in the second copy.
    yes = map (True:)
    bss = bools (n-1)

p1 :: Prop
p1 = And (Var 'A') (Not (Var 'A'))

p2 :: Prop
p2 = Imply (And (Var 'A') (Var 'B')) (Var 'A')

p3 :: Prop
p3 = Imply (Var 'A') (And (Var 'A') (Var 'B'))

p4 :: Prop
p4 = Imply (And (Var 'A') (Imply
        (Var 'A') (Var 'B'))) (Var 'B')
