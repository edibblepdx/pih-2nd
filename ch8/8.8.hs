-- Exercise 8.8: Extend the tautology checker to support the use of logical
-- disjunciton and equivalence in propositions.

-- A proposition
data Prop
  = Const Bool
  | Var Char
  | Not Prop -- not
  | And Prop Prop -- conjunciton
  | Or Prop Prop -- disjunciton
  | Eq Prop Prop -- equivalence
  | Imply Prop Prop -- implication

-- An association
type Assoc k v = [(k, v)]

find :: (Eq k) => k -> Assoc k v -> v
find k t = head [v | (k', v) <- t, k == k']

-- A substitution
type Subst = Assoc Char Bool

eval :: Subst -> Prop -> Bool
eval _ (Const b) = b
eval s (Var x) = find x s
eval s (Not p) = not (eval s p)
eval s (And p q) = eval s p && eval s q
eval s (Imply p q) = eval s p <= eval s q
eval s (Or p q) = eval s p || eval s q
eval s (Eq p q) = eval s p == eval s q

vars :: Prop -> [Char]
vars (Const _) = []
vars (Var x) = [x]
vars (Not p) = vars p
vars (And p q) = vars p ++ vars q
vars (Imply p q) = vars p ++ vars q
vars (Or p q) = vars p ++ vars q
vars (Eq p q) = vars p ++ vars q

bools :: Int -> [[Bool]]
bools 0 = [[]]
bools n = map (False :) bss ++ map (True :) bss
  where
    bss = bools (n - 1)

rmdups :: (Eq a) => [a] -> [a]
rmdups [] = []
rmdups (x : xs) = x : filter (/= x) (rmdups xs)

substs :: Prop -> [Subst]
substs p = map (zip vs) (bools (length vs))
  where
    vs = rmdups (vars p)

isTaut :: Prop -> Bool
isTaut p = and [eval s p | s <- substs p]

p1 :: Prop -- True
p1 = Imply (Var 'A') (Or (Var 'A') (Var 'B'))

p2 :: Prop -- True
p2 = Eq (Not (And (Var 'A') (Var 'B'))) (Or (Not (Var 'A')) (Not (Var 'B')))

p3 :: Prop -- False
p3 = Eq (Var 'A') (Var 'B')

p4 :: Prop -- False
p4 = Or (Var 'A') (Var 'B')
