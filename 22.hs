data Nat = Zero | Suc Nat

fromNat :: Nat -> Integer
fromNat Zero    = 0
fromNat (Suc n) = fromNat n + 1

add :: Nat -> Nat -> Nat
x `add` Zero    = x
x `add` (Suc y) = Suc (x `add` y) 

mul :: Nat -> Nat -> Nat
_ `mul` Zero       = Zero
x `mul` y@(Suc y1) = (x `mul` y1) `add` x

fac :: Nat -> Nat
fac Zero       = Suc Zero
fac x@(Suc x1) = x `mul` fac x1