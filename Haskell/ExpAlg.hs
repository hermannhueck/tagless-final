module ExpAlg where

{-
    An object algebra allows us to extend in 2 dimensions without loosing type safety.
    It is basically the same as tagless final with FP.

    The code is heavily based on type classes what avoid wiring the traits/classes by inheritence.

    A few words about the terminology. The abstract factory interface ExpAlg<T> is called an object algebra interface
    and concrete factories implementing it are called object algebras. The terminology and the approach in general are
    inspired by abstract algebra, in which algebraic structures (or simply algebras) are described by signatures. A signature
    acts as an interface that specifies the types of operations defined on the underlying set of an algebraic structure,
    and an algebra provides concrete definitions of those operations and is similar to a class implementing an interface.
-}
class ExpAlg t where
    lit :: Int -> t
    add :: t -> t -> t


newtype Eval = Eval { eval :: Int }

instance ExpAlg Eval where
    lit n   = Eval n
    add x y = Eval $ eval x + eval y


-- --- In order to add a new operation 'mul' we are
-- --- extending the Algebra ExpAlg with multiplication
--
class ExpAlg t => MulAlg t where
    mul :: t -> t -> t

instance MulAlg Eval where
    mul x y = Eval $ eval x * eval y


-- --- Adding a new interpreter 'view'
--
newtype View = View { view :: String }

instance ExpAlg View where
    lit n   = View $ show n
    add x y = View $ "(" ++ view x ++ " + " ++ view y ++ ")"

instance MulAlg View where
    mul x y = View $ "(" ++ view x ++ " * " ++ view y ++ ")"



e1 :: ExpAlg t => t
e1 = add (lit 1)
        (add (lit 2)
             (lit 3))

e2 :: MulAlg t => t
e2 = mul (lit 4)
         (add (lit 5)
              (lit 6))



main :: IO ()
main = do
  putStrLn "==========="
  print $ eval e1
  print $ eval e2
  print $ view e1
  print $ view e2
  putStrLn "==========="
