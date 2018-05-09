module Exp where

{-
    The expression problem:
    We cannot extend existing code in two dimensions: operations and interpreters

    In the OO approach we can easily add a new operation (class Mul), but adding a new interpreter (view())
    would break existing code, because the new method view must be added to the interface Exp and all classes
    implementing this interfaces.

    In the FP approach adding an interpreter is straight forward, but adding a new operation would break
    existing code.
-}
data Exp = Lit Int | Add Exp Exp

eval :: Exp -> Int
eval (Lit n)   = n
eval (Add x y) = eval x + eval y

view :: Exp -> String
view (Lit n)   = show n
view (Add x y) = "(" ++ view x ++ " + " ++ view y ++ ")"



e :: Exp
e = Add (Lit 1)
        (Add (Lit 2)
            (Lit 3))



main :: IO ()
main = do
    putStrLn "==========="
    print $ eval e
    print $ view e
    putStrLn "==========="
