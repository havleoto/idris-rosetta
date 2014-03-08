--
-- Task: Factorial
--
-- Task Description: Write a function to return the factorial of
-- a number. Solutions can be iterative or recursive. Support for
-- trapping negative n errors is optional. 
--
-- The Idris module Prelude.Nat defines the factorial function as
--
--  total fact : Nat -> Nat
--  fact Z     = S Z
--  fact (S n) = (S n) * fact n
--

%default total

-- Define a tail recursive function tailFact.

tailFact : Nat -> Nat
tailFactGo : Nat -> Nat -> Nat
tailFact n = tailFactGo 1 n
tailFactGo a Z = a
tailFactGo a (S n) = tailFactGo (a * S n) n

-- Prove that (tailFact n = fact n) for all n:Nat.

thm_tailFact : (n:Nat) -> tailFact n = fact n
thm_tailFact n =
  let lemma = thm_go 1 n in
    ?thm_tailFactMain
  where
    thm_go : (a:Nat) -> (n:Nat) -> tailFactGo a n = a * fact n
    thm_go a Z = sym $ multOneRightNeutral a
    thm_go a (S n) = let IH = thm_go (a * S n) n in ?thm_tailFactStep

thm_tailFactMain = proof
  intros
  rewrite (sym lemma)
  exact (plusZeroRightNeutral _)

thm_tailFactStep = proof
  intros
  rewrite (sym IH)
  rewrite (multAssociative a (S n) (fact n))
  trivial

-- Another definition without proofs.

sugarFact : Nat -> Nat
sugarFact n = product [1..n]
